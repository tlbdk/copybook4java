package dk.nversion.copybook.serializers;

import dk.nversion.copybook.exceptions.CopyBookException;

import java.lang.reflect.Array;
import java.nio.ByteBuffer;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class FullMapper extends CopyBookMapperBase {

    public <T> byte[] serialize(T obj) {
        ByteBuffer buffer = ByteBuffer.wrap(new byte[this.maxRecordSize]);
        writeFieldsToBuffer(this.fields, buffer, obj);
        byte[] result = new byte[buffer.position()];
        System.arraycopy(buffer.array(), 0, result, 0, buffer.position()); // Copy bytes to result array
        return result;
    }

    private <T> void writeFieldsToBuffer(List<CopyBookField> fields, ByteBuffer buffer, T rootObj) {
        for(CopyBookField field : fields) {
            if(field.isArray()) {
                Object array = field.getObject(rootObj);
                int arraySize = Math.max(array != null ? Array.getLength(array) : 0, field.getMinOccurs());
                if(field.hasSubCopyBookFields()) {
                    // Complex array types fx. Request[]
                    for (int i = 0; i < arraySize; i++) {
                        writeFieldsToBuffer(field.getSubCopyBookFields(), buffer, field.getObject(rootObj, i));
                    }

                } else {
                    // Simple array types, fx. int[]
                    for (int i = 0; i < arraySize; i++) {
                        buffer.put(field.getBytes(rootObj, array, i, true));
                    }
                }

            } else if(field.hasSubCopyBookFields()) {
                // Complex type fx, Request
                writeFieldsToBuffer(field.getSubCopyBookFields(), buffer, field.getObject(rootObj));

            } else {
                // Simple type fx. int
                buffer.put(field.getBytes(rootObj, true));
            }
        }
    }

    public <T> T deserialize(byte[] bytes, Class<T> type) {
        try {
            T obj = type.newInstance();
            ByteBuffer buffer = ByteBuffer.wrap(bytes);
            readFieldsFromBuffer(this.fields, buffer, obj, "", new HashMap<>());
            return obj;

        } catch (IllegalAccessException | InstantiationException e) {
            throw new CopyBookException("Failed to create new object", e);
        }
    }

    private void readFieldsFromBuffer(List<CopyBookField> fields, ByteBuffer buffer, Object obj, String name, Map<String, Integer> counters) {
        for(CopyBookField field : fields) {
            String fieldName = name + "." + field.getFieldName();
            if(field.isArray()) {
                int arraySize = field.getMaxOccurs();

                // Support field ending in _count as counter keys
                if(counters.containsKey(fieldName + "_count")) {
                    arraySize = counters.get(fieldName + "_count");

                // Support depending on cobol syntax for counters
                } else if(counters.containsKey(field.getCounterKey())) {
                    arraySize = counters.get(field.getCounterKey());
                }

                Object array = field.createArrayObject(obj, arraySize);
                if(field.hasSubCopyBookFields()) {
                    // Complex array types fx. Request[]
                    for (int i = 0; i < arraySize; i++) {
                        readFieldsFromBuffer(field.getSubCopyBookFields(), buffer, field.createObject(array, i), fieldName, counters);
                    }

                    // Move position in buffer to next location with data
                    if(field.getMinOccurs() > arraySize) {
                        int skipSize = (field.getMinOccurs() - arraySize) * field.getRecursiveMinSize() / field.getMinOccurs();
                        buffer.position(buffer.position() + skipSize);
                    }

                } else {
                    // Simple array types, fx. int[]
                    for (int i = 0; i < arraySize; i++) {
                        field.setBytes(array, i, buffer, true);
                    }

                    // Move position in buffer to next location with data
                    if(field.getMinOccurs() > arraySize) {
                        buffer.position(buffer.position() + (field.getMinOccurs() - arraySize) * field.getSize());
                    }
                }

            } else if(field.hasSubCopyBookFields()) {
                // Complex type fx, Request
                readFieldsFromBuffer(field.getSubCopyBookFields(), buffer, field.createObject(obj), fieldName, counters);

            } else {
                // Simple type fx. int, String or types we support with TypeConverters
                Object value = field.setBytes(obj, buffer, true);

                // Save field name and counter value for later use
                if(value != null) {
                    if (field.getField().getType().equals(Integer.TYPE) && fieldName.endsWith("_count")) {
                        counters.put(fieldName, (Integer) value);
                    }
                    if (field.isCounter()) {
                        counters.put(field.getName(), (Integer)value);
                    }
                }
            }
        }
    }
}
