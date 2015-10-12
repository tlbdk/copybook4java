package dk.nversion.copybook.serializers;

import dk.nversion.copybook.converters.TypeConverterBase;

import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.util.List;

public class CopyBookField {
    private List<CopyBookField> subCopyBookFields;
    private Field field;
    private String[] lines;
    private TypeConverterBase converter;
    private String name;
    private int size;
    private int decimals;
    private int minOccurs;
    private int maxOccurs;
    private String counterKey;

    public boolean isArray() {
        return field.getType().isArray();
    }

    public CopyBookField(Field field, String name, int size, int decimals, int minOccurs, int maxOccurs, String[] lines, String counterKey, TypeConverterBase converter) {
        // Handle private fields
        if(!field.isAccessible()) {
            field.setAccessible(true);
        }

        this.field = field;
        this.lines = lines;
        this.counterKey = counterKey;
        this.converter = converter;
        this.name = name;
        this.size = size;
        this.decimals = decimals;
        this.minOccurs = minOccurs;
        this.maxOccurs = maxOccurs;
    }

    public void setBytes(Object obj, byte[] bytes, int offset, int length, boolean removePadding) {
        Object value = converter.to(bytes, offset, length, removePadding);
        try {
            field.set(obj, value);
        } catch (IllegalAccessException ex) {
            // We already set it to accessible so this should not happen
        }
    }

    public void setBytes(Object obj, int index, byte[] bytes, int offset, int length, boolean removePadding) {
        Object value = converter.to(bytes, offset, length, removePadding);
        try {
            Object array = field.get(obj);
            Array.set(array, index, value);
            field.set(obj, value);
        } catch (IllegalAccessException ex) {
            // We already set it to accessible so this should not happen
        }
    }

    public byte[] getBytes(Object obj, boolean addPadding) {
        try {
            return converter.from(field.get(obj), addPadding);
        } catch (IllegalAccessException ex) {
            // We already set it to accessible so this should not happen
            return null;
        }
    }

    public byte[] getBytes(Object obj, int index, boolean addPadding) {
        try {
            Object array = field.get(obj);
            return converter.from(Array.get(array, index), addPadding);
        } catch (IllegalAccessException ex) {
            // We already set it to accessible so this should not happen
            return null;
        }
    }

    public List<CopyBookField> getSubCopyBookFields() {
        return subCopyBookFields;
    }

    public void setSubCopyBookFields(List<CopyBookField> subCopyBookFields) {
        this.subCopyBookFields = subCopyBookFields;
    }

    public Field getField() {
        return field;
    }

    public void setField(Field field) {
        this.field = field;
    }

    public String[] getLines() {
        return lines;
    }

    public void setLines(String[] lines) {
        this.lines = lines;
    }

    public TypeConverterBase getConverter() {
        return converter;
    }

    public void setConverter(TypeConverterBase converter) {
        this.converter = converter;
    }

    public int getSize() {
        return size;
    }

    public void setSize(int size) {
        this.size = size;
    }

    public int getDecimals() {
        return decimals;
    }

    public void setDecimals(int decimals) {
        this.decimals = decimals;
    }

    public int getMinOccurs() {
        return minOccurs;
    }

    public void setMinOccurs(int minOccurs) {
        this.minOccurs = minOccurs;
    }

    public int getMaxOccurs() {
        return maxOccurs;
    }

    public void setMaxOccurs(int maxOccurs) {
        this.maxOccurs = maxOccurs;
    }

    public String getCounterKey() {
        return counterKey;
    }

    public void setCounterKey(String counterKey) {
        this.counterKey = counterKey;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }
}
