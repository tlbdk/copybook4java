package dk.nversion.copybook.serializers;


import dk.nversion.copybook.exceptions.CopyBookException;

import java.util.List;

public abstract class CopyBookMapperBase implements CopyBookMapper {
    protected List<CopyBookField> fields;
    protected boolean debug;
    protected int maxRecordSize;
    protected int minRecordSize;

    @Override
    public void initialize(CopyBookSerializerConfig config) {
        this.fields = config.getFields();
        this.debug = config.isDebug();
        int[] totalSizes = calculateSizes(config.getFields(), 0, this.debug);
        this.minRecordSize = totalSizes[0];
        this.maxRecordSize = totalSizes[1];
    }

    protected int[] calculateSizes(List<CopyBookField> fields, int level, boolean debug) {
        int minTotal = 0;
        int maxTotal = 0;
        for(int i = 0; i < fields.size(); i++) {
            CopyBookField field = fields.get(i);

            if(debug) {
                for (String line : field.getLines()) {
                    System.out.println(new String(new char[level * 2]).replace("\0", " ") + line);
                }
            }

            int minSize;
            int maxSize;
            if(field.isArray()) {
                if(field.hasSubCopyBookFields()) {
                    // Complex array types fx. Request[]
                    int[] sizes = calculateSizes(field.getSubCopyBookFields(), level + 1, debug);
                    minSize = sizes[0] * field.getMinOccurs();
                    maxSize = sizes[1] * field.getMaxOccurs();

                } else {
                    // Simple array types, fx. int[]
                    minSize = field.getSize() * field.getMinOccurs();
                    maxSize = field.getSize() * field.getMaxOccurs();
                }

            } else if(field.hasSubCopyBookFields()) {
                // Complex type fx, Request
                int[] sizes = calculateSizes(field.getSubCopyBookFields(), level + 1, debug);
                minSize = sizes[0];
                maxSize = sizes[1];

            } else {
                // Simple type fx. int, String or types we support with TypeConverters
                minSize = field.getSize();
                maxSize = minSize;
            }
            minTotal += minSize;
            maxTotal += maxSize;

            field.setRecursiveMinSize(minSize);
            field.setRecursiveMaxSize(maxSize);
            field.setLevel(level);
            field.setLast(fields.size() - 1 == i);
        }

        return new int[] { minTotal, maxTotal };
    }

    public int getMinRecordSize() {
        return minRecordSize;
    }

    public void setMinRecordSize(int minRecordSize) {
        this.minRecordSize = minRecordSize;
    }

    public int getMaxRecordSize() {
        return maxRecordSize;
    }

    public void setMaxRecordSize(int maxRecordSize) {
        this.maxRecordSize = maxRecordSize;
    }
}
