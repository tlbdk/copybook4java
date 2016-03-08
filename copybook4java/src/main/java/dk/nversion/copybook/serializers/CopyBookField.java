/*
 * Copyright (c) 2015. Troels Liebe Bentsen <tlb@nversion.dk>
 * Licensed under the MIT license (LICENSE.txt)
 */

package dk.nversion.copybook.serializers;

import dk.nversion.copybook.converters.TypeConverter;
import dk.nversion.copybook.exceptions.CopyBookException;
import dk.nversion.copybook.exceptions.TypeConverterException;

import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.nio.ByteBuffer;
import java.util.List;

public class CopyBookField {
    private List<CopyBookField> subCopyBookFields;
    // TODO: Change to types and Class[]
    private Class<?> type;
    private Field field;
    private String[] lines;
    // TODO: Change to converters and TypeConverterBase[]
    private TypeConverter converter;
    private String name;
    private int size;
    private int decimals;
    private int minOccurs;
    private int maxOccurs;
    private String counterKey;
    private boolean counter;

    private String redefines;
    private String redefinedOn;
    private String redefineMatch;

    private int recursiveMaxSize;
    private int recursiveMinSize;
    private boolean last;
    private int level;

    public boolean isArray() {
        return field.getType().isArray();
    }

    public CopyBookField(Class<?> type, Field field, String name, int size, int decimals, int minOccurs, int maxOccurs, String[] lines, String counterKey, TypeConverter converter) {
        // Handle private fields
        if(!field.isAccessible()) {
            field.setAccessible(true);
        }

        this.type = type;
        this.field = field;
        this.lines = lines.clone();
        this.counterKey = counterKey;
        this.converter = converter;
        this.name = name;
        this.size = size;
        this.decimals = decimals;
        this.minOccurs = minOccurs;
        this.maxOccurs = maxOccurs;
    }

    public Object setBytes(Object obj, ByteBuffer buffer, boolean removePadding) {
        byte[] bytes = new byte[this.size];
        buffer.get(bytes);
        return setBytes(obj, bytes, 0,  bytes.length, removePadding);
    }

    public Object setBytes(Object obj, byte[] bytes, boolean removePadding) {
        return setBytes(obj, bytes, 0, bytes != null ? bytes.length : 0, removePadding);
    }

    public Object setBytes(Object obj, byte[] bytes, int offset, int length, boolean removePadding) {
        try {
            Object value = bytes != null ? converter.to(bytes, offset, length, this.decimals, removePadding) : null;
            field.set(obj, value);
            return value;

        } catch (IllegalAccessException ex) {
            // We already set it to accessible so this should not happen
            return null;

        } catch (Exception ex) {
            throw new CopyBookException(getFieldName() + ": ", ex);
        }
    }

    public Object setBytes(Object obj, int index, ByteBuffer buffer, boolean removePadding) {
        byte[] bytes = new byte[this.size];
        buffer.get(bytes);
        return setBytes(obj, index, bytes, 0, bytes.length, removePadding);
    }

    public Object setBytes(Object arrayObj, int index, byte[] bytes, boolean removePadding) {
        return setBytes(arrayObj, index, bytes, 0, bytes != null ? bytes.length : 0, removePadding);
    }

    public Object setBytes(Object arrayObj, int index, byte[] bytes, int offset, int length, boolean removePadding) {
        try {
            Object value = bytes != null ? converter.to(bytes, offset, length, this.decimals, removePadding) : null;
            Array.set(arrayObj, index, value);
            return value;

        } catch (Exception ex) {
            throw new CopyBookException(getFieldName() + ": ", ex);
        }
    }

    public byte[] getBytes(Object rootObj, boolean addPadding) {
        return getBytes(rootObj, null, addPadding);
    }

    public byte[] getBytes(Object rootObj, Object valueObj, boolean addPadding) {
        try {
            if(valueObj == null) {
                valueObj = getObject(rootObj);
            }
            return converter.from(valueObj, this.size, this.decimals, addPadding);
            
        } catch (Exception ex) {
            throw new CopyBookException(getFieldName() + ": ", ex);
        }
    }

    public byte[] getBytes(Object rootObj, int index, boolean addPadding) {
       return getBytes(rootObj, null, index, addPadding);
    }

    public byte[] getBytes(Object rootObj, Object arrayObj, int index, boolean addPadding) {
        try {
            return converter.from(getObject(rootObj, arrayObj, index), this.size, this.decimals, addPadding);

        } catch (Exception ex) {
            throw new CopyBookException(getFieldName() + ": ", ex);
        }
    }

    public Object getObject(Object rootObj) {
        try {
            return rootObj != null ? field.get(rootObj) : null;

        } catch (IllegalAccessException e) {
            // We already set it to accessible so this should not happen
            return null;
        }
    }

    public Object getObject(Object rootObj, int index) {
        Object array = getObject(rootObj);
        return array != null && index < Array.getLength(array) ? Array.get(array, index) : null;
    }

    public Object getObject(Object rootObj, Object arrayObj, int index) {
        if(arrayObj == null) {
            arrayObj = getObject(rootObj);
        }
        return arrayObj != null && index < Array.getLength(arrayObj) ? Array.get(arrayObj, index) : null;
    }

    public Object createArrayObject(Object rootObj, int size) {
        try {
            Object array = Array.newInstance(this.field.getType().getComponentType(), size);
            this.field.set(rootObj, array);
            return array;

        } catch (IllegalAccessException e) {
            // We already set it to accessible so this should not happen
            return null;
        }
    }

    public Object createObject(Object rootObj) {
        try {
            Object value = field.getType().newInstance();
            this.field.set(rootObj, value);
            return value;

        } catch (IllegalAccessException e) {
            // We already set it to accessible so this should not happen
            return null;

        } catch (InstantiationException e) {
            throw new CopyBookException("Failed to create new object", e);
        }
    }

    public Object createObject(Object obj, int index) {
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
        return lines.clone();
    }

    public void setLines(String[] lines) {
        this.lines = lines.clone();
    }

    public TypeConverter getConverter() {
        return converter;
    }

    public void setConverter(TypeConverter converter) {
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
        return counter;
    }

    public void setCounter(boolean counter) {
        this.counter = counter;
    }

    public String getRedefines() {
        return redefines;
    }

    public void setRedefines(String redefines) {
        this.redefines = redefines;
    }

    public String getRedefinedOn() {
        return redefinedOn;
    }

    public void setRedefinedOn(String redefinedOn) {
        this.redefinedOn = redefinedOn;
    }

    public String getRedefineMatch() {
        return redefineMatch;
    }

    public void setRedefineMatch(String redefineMatch) {
        this.redefineMatch = redefineMatch;
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
