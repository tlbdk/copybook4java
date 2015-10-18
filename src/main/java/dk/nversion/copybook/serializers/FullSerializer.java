package dk.nversion.copybook.serializers;

import dk.nversion.copybook.exceptions.CopyBookException;

import java.lang.reflect.Array;
import java.nio.ByteBuffer;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class FullSerializer extends CopyBookSerializerBase {
    private int maxRecordSize;
    private Map<CopyBookField, Integer> fieldRecursiveSizes = new HashMap<>();

    public FullSerializer(CopyBookSerializerConfig config) {
        super(config);
        this.maxRecordSize = calculateMaxSize(config.getFields(), 0, this.debug);
    }

    public <T> byte[] serialize(T obj) throws CopyBookException {
        ByteBuffer buffer = ByteBuffer.wrap(new byte[this.maxRecordSize]);
        writeFieldsToBuffer(this.fields, buffer, obj);
        return buffer.array();
    }

    private <T> void writeFieldsToBuffer(List<CopyBookField> fields, ByteBuffer buffer, T obj) throws CopyBookException {
        for(CopyBookField field : fields) {
            if(field.isArray()) {
                if(field.hasSubCopyBookFields()) {
                    // Complex array types fx. Request[]
                    for (int i = 0; i < field.getMaxOccurs(); i++) {
                        writeFieldsToBuffer(field.getSubCopyBookFields(), buffer, field.getObject(obj, i));
                    }

                } else {
                    // Simple array types, fx. int[]
                    for (int i = 0; i < field.getMaxOccurs(); i++) {
                        buffer.put(field.getBytes(obj, i, true));
                    }
                }

            } else if(field.hasSubCopyBookFields()) {
                // Complex type fx, Request
                writeFieldsToBuffer(field.getSubCopyBookFields(), buffer, field.getObject(obj));

            } else {
                // Simple type fx. int
                buffer.put(field.getBytes(obj, true));
            }
        }
    }

    public <T> T deserialize(byte[] bytes, Class<T> type) throws CopyBookException {
        try {
            T obj = type.newInstance();
            ByteBuffer buffer = ByteBuffer.wrap(bytes);
            readFieldsFromBuffer(this.fields, buffer, obj, "", new HashMap<>());
            return obj;

        } catch (IllegalAccessException | InstantiationException e) {
            throw new CopyBookException("Failed to create new object", e);
        }
    }

    private void readFieldsFromBuffer(List<CopyBookField> fields, ByteBuffer buffer, Object obj, String name, Map<String, Integer> counters) throws CopyBookException {
        for(CopyBookField field : fields) {
            String fieldName = name + "." + field.getFieldName();
            if(field.isArray()) {
                // Support field naming as counter keys
                int arraySize = field.getMaxOccurs();
                if(counters.containsKey(fieldName + "_count")) {
                    arraySize = counters.get(fieldName + "_count");

                // Support depending on cobol syntax for counters
                } else if(counters.containsKey(field.getName())) {
                    arraySize = counters.get(field.getName());
                }

                Object array = field.createArrayObject(obj, arraySize);
                if(field.hasSubCopyBookFields()) {
                    // Complex array types fx. Request[]
                    for (int i = 0; i < arraySize; i++) {
                        readFieldsFromBuffer(field.getSubCopyBookFields(), buffer, field.createObject(array, i), fieldName, counters);
                    }

                    // Move position in buffer to next location with data
                    if(field.getMinOccurs() > arraySize) {
                        int skipSize = (field.getMaxOccurs() - arraySize) * this.fieldRecursiveSizes.get(field) / field.getMaxOccurs();
                        buffer.position(buffer.position() + skipSize);
                    }

                } else {
                    // Simple array types, fx. int[]
                    for (int i = 0; i < arraySize; i++) {
                        field.setBytes(array, i, buffer, true);
                    }

                    // Move position in buffer to next location with data
                    if(field.getMinOccurs() > arraySize) {
                        buffer.position(buffer.position() + (field.getMaxOccurs() - arraySize) * field.getSize());
                    }
                }

            } else if(field.hasSubCopyBookFields()) {
                // Complex type fx, Request
                readFieldsFromBuffer(field.getSubCopyBookFields(), buffer, field.createObject(obj), fieldName, counters);

            } else {
                // Simple type fx. int, String or types we support with TypeConverters
                Object value = field.setBytes(obj, buffer, true);

                // Save field name and counter value for later use
                if(field.getField().getType().equals(Integer.TYPE) && fieldName.endsWith("_count")) {
                    counters.put(fieldName, (int)value);
                }

                if(field.isCounter()) {
                    counters.put(field.getName(), (int)value);
                }
            }
        }
    }

}
