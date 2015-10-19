package dk.nversion.copybook.serializers;

import dk.nversion.copybook.exceptions.CopyBookException;
import dk.nversion.copybook.converters.TypeConverterBase;
import dk.nversion.copybook.exceptions.TypeConverterException;

import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.nio.ByteBuffer;
import java.util.List;

public class CopyBookField {
    private List<CopyBookField> subCopyBookFields;
    private Class type;
    private Field field;
    private String[] lines;
    private TypeConverterBase converter;
    private String name;
    private int size;
    private int decimals;
    private int minOccurs;
    private int maxOccurs;
    private String counterKey;
    private boolean isCounter;

    private int recursiveMinSize;
    private int recursiveMaxSize;
    private boolean last;
    private int level;

    public boolean isArray() {
        return field.getType().isArray();
    }

    public CopyBookField(Class type, Field field, String name, int size, int decimals, int minOccurs, int maxOccurs, String[] lines, String counterKey, TypeConverterBase converter) {
        // Handle private fields
        if(!field.isAccessible()) {
            field.setAccessible(true);
        }

        this.type = type;
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

    public Object setBytes(Object obj, ByteBuffer buffer, boolean removePadding) throws CopyBookException {
        byte[] bytes = new byte[this.size];
        buffer.get(bytes);
        return setBytes(obj, bytes, 0, bytes.length, removePadding);
    }

    public Object setBytes(Object obj, byte[] bytes, int offset, int length, boolean removePadding) throws CopyBookException {
        try {
            Object value = converter.to(bytes, offset, length, this.decimals, removePadding);
            field.set(obj, value);
            return value;

        } catch (IllegalAccessException ex) {
            // We already set it to accessible so this should not happen
            return null;

        } catch (TypeConverterException ex) {
            throw new CopyBookException(getFieldName() + ": ", ex);
        }
    }

    public Object setBytes(Object obj, int index, ByteBuffer buffer, boolean removePadding) throws CopyBookException {
        byte[] bytes = new byte[this.size];
        buffer.get(bytes);
        return setBytes(obj, index, bytes, 0, bytes.length, removePadding);
    }

    public Object setBytes(Object arrayObj, int index, byte[] bytes, int offset, int length, boolean removePadding) throws CopyBookException {
        try {
            Object value = converter.to(bytes, offset, length, this.decimals, removePadding);
            Array.set(arrayObj, index, value);
            return value;

        } catch (TypeConverterException ex) {
            throw new CopyBookException(getFieldName() + ": ", ex);
        }
    }

    public byte[] getBytes(Object obj, boolean addPadding) throws CopyBookException {
        try {
            return converter.from(getObject(obj), this.size, this.decimals, addPadding);
            
        } catch (TypeConverterException ex) {
            throw new CopyBookException(getFieldName() + ": ", ex);
        } catch (Exception ex) {
            throw ex;
        }
    }

    public byte[] getBytes(Object obj, int index, boolean addPadding) throws CopyBookException {
        try {
           return converter.from(getObject(obj, index), this.size, this.decimals, addPadding);

        } catch (TypeConverterException ex) {
            throw new CopyBookException(getFieldName() + ": ", ex);
        }
    }

    public Object getObject(Object obj) throws CopyBookException {
        try {
            return obj != null ? field.get(obj) : null;

        } catch (IllegalAccessException e) {
            // We already set it to accessible so this should not happen
            return null;
        }
    }

    public Object getObject(Object obj, int index) throws CopyBookException {
        Object array = getObject(obj);
        return array != null && index < Array.getLength(array) ? Array.get(array, index) : null;
    }

    public Object createArrayObject(Object obj, int size) throws CopyBookException {
        try {
            Object array = Array.newInstance(this.field.getType().getComponentType(), size);
            this.field.set(obj, array);
            return array;

        } catch (IllegalAccessException e) {
            // We already set it to accessible so this should not happen
            return null;
        }
    }

    public Object createObject(Object obj) throws CopyBookException {
        try {
            Object value = field.getType().newInstance();
            this.field.set(obj, value);
            return value;

        } catch (IllegalAccessException e) {
            // We already set it to accessible so this should not happen
            return null;

        } catch (InstantiationException e) {
            throw new CopyBookException("Failed to create new object", e);
        }
    }

    public Object createObject(Object obj, int index) throws CopyBookException {
        try {
            Object value = field.getType().getComponentType().newInstance();
            Array.set(obj, index, value);
            return value;

        } catch (IllegalAccessException e) {
            // We already set it to accessible so this should not happen
            return null;

        } catch (InstantiationException e) {
            throw new CopyBookException("Failed to create new object", e);
        }
    }

    public boolean hasSubCopyBookFields() {
        return this.subCopyBookFields != null && this.subCopyBookFields.size() > 0;
    }

    public List<CopyBookField> getSubCopyBookFields() {
        return subCopyBookFields;
    }

    public void setSubCopyBookFields(List<CopyBookField> subCopyBookFields) {
        this.subCopyBookFields = subCopyBookFields;
    }

    public String getFieldName() {
        return this.type.getName() + "." + this.field.getName();
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

    public boolean isCounter() {
        return isCounter;
    }

    public void setIsCounter(boolean isCounter) {
        this.isCounter = isCounter;
    }

    public int getRecursiveMinSize() {
        return recursiveMinSize;
    }

    public void setRecursiveMinSize(int recursiveMinSize) {
        this.recursiveMinSize = recursiveMinSize;
    }

    public boolean isLast() {
        return last;
    }

    public void setLast(boolean last) {
        this.last = last;
    }

    public int getRecursiveMaxSize() {
        return recursiveMaxSize;
    }

    public void setRecursiveMaxSize(int recursiveMaxSize) {
        this.recursiveMaxSize = recursiveMaxSize;
    }

    public int getLevel() {
        return level;
    }

    public void setLevel(int level) {
        this.level = level;
    }
}
