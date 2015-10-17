package dk.nversion.copybook.serializers;

import dk.nversion.copybook.exceptions.CopyBookException;

import java.lang.reflect.Array;
import java.nio.ByteBuffer;
import java.util.List;

public class FullSerializer extends CopyBookSerializerBase {
    private int maxRecordSize;

    public FullSerializer(CopyBookSerializerConfig config) {
        super(config);
        this.maxRecordSize = calculateMaxSize(config.getFields(), 0, this.debug);
    }

    private int calculateMaxSize(List<CopyBookField> fields, int level, boolean debug) {
        int size = 0;
        for(CopyBookField field : fields) {
            if(field.isArray()) {
                if(field.hasSubCopyBookFields()) {
                    size += calculateMaxSize(field.getSubCopyBookFields(), level + 1, debug) * field.getMaxOccurs();

                } else {
                    size += field.getSize() * field.getMaxOccurs();
                }

            } else if(field.hasSubCopyBookFields()) {
                size += calculateMaxSize(field.getSubCopyBookFields(), level + 1, debug);

            } else {
                size += field.getSize();
            }

            if(debug) {
                for (String line : field.getLines()) {
                    System.out.println(new String(new char[level * 2]).replace("\0", " ") + line + " ( " + size + ")");
                }
            }
        }
        return size;
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
            readFieldsFromBuffer(this.fields, buffer, obj);
            return obj;

        } catch (IllegalAccessException | InstantiationException e) {
            throw new CopyBookException("Failed to create new object", e);
        }
    }

    private void readFieldsFromBuffer(List<CopyBookField> fields, ByteBuffer buffer, Object obj) throws CopyBookException {
        for(CopyBookField field : fields) {
            if(field.isArray()) {
                if(field.hasSubCopyBookFields()) {
                    // Complex array types fx. Request[]
                    Object array = field.createArrayObject(obj, field.getMaxOccurs()); // TODO: Calculate size hint
                    for (int i = 0; i < field.getMaxOccurs(); i++) {
                        readFieldsFromBuffer(field.getSubCopyBookFields(), buffer, field.createObject(array, i));
                    }

                } else {
                    // Simple array types, fx. int[]
                    Object array = field.createArrayObject(obj, field.getMaxOccurs()); // TODO: Calculate size hint
                    for (int i = 0; i < field.getMaxOccurs(); i++) {
                        field.setBytes(array, i, buffer, true);
                    }
                }

            } else if(field.hasSubCopyBookFields()) {
                // Complex type fx, Request
                readFieldsFromBuffer(field.getSubCopyBookFields(), buffer, field.createObject(obj));

            } else {
                // Simple type fx. int, String or types we support with TypeConverts
                field.setBytes(obj, buffer, true);
            }
        }
    }

}
