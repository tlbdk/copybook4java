package dk.nversion.copybook.serializers;


import dk.nversion.copybook.exceptions.CopyBookException;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public abstract class CopyBookSerializerBase {
    protected CopyBookSerializerConfig config;
    protected List<CopyBookField> fields;
    protected boolean debug;
    private Map<CopyBookField, Integer> fieldRecursiveSizes = new HashMap<>();

    public CopyBookSerializerBase(CopyBookSerializerConfig config) {
        this.config = config;
        this.fields = config.getFields();
        this.debug = config.isDebug();
    }

    public abstract <T> byte[] serialize(T obj) throws CopyBookException;
    public abstract <T> T deserialize(byte[] bytes, Class<T> type) throws CopyBookException, InstantiationException;

    protected int calculateMaxSize(List<CopyBookField> fields, int level, boolean debug) {
        int result = 0;
        // TODO: set as part of this field.setLast();
        // TODO: set level on field.setLevel();
        for(CopyBookField field : fields) {

            if(debug) {
                for (String line : field.getLines()) {
                    System.out.println(new String(new char[level * 2]).replace("\0", " ") + line);
                }
            }
            int size;
            if(field.isArray()) {
                if(field.hasSubCopyBookFields()) {
                    size = calculateMaxSize(field.getSubCopyBookFields(), level + 1, debug) * field.getMaxOccurs();

                } else {
                    size = field.getSize() * field.getMaxOccurs();
                }

            } else if(field.hasSubCopyBookFields()) {
                size = calculateMaxSize(field.getSubCopyBookFields(), level + 1, debug);

            } else {
                size = field.getSize();
            }

            // TODO: Use field.setRecursiveMinSize() instead of hashmap
            // TODO: Also do a field.setRecursiveMaxSize()
            this.fieldRecursiveSizes.put(field, size);
            result += size;
        }
        return result;
    }
}
