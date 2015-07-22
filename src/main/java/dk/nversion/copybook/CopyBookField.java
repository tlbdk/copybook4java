package dk.nversion.copybook;

import dk.nversion.ByteUtils;

import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.nio.charset.Charset;
import java.util.Arrays;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class CopyBookField {
    private Pattern re_pictype = Pattern.compile("(X|9|S9)\\((\\d+)\\)(?:V9\\((\\d+)\\))?\\.");

    public CopyBookFieldType type;
    public int offset;
    public int size;
    public int decimal;
    public Field[] fields;
    public int[] indexes;
    public int[] occurs;
    public CopyBookField[] counters;
    public String line;

    public boolean rightPadding;
    public byte padding;
    public Charset charset;

    public boolean packingItem;

    public CopyBookField() {

    }

    public CopyBookField(String copybookLine, Charset charset, Field[] fields, CopyBookField[] counters, int[] indexes, int[] occurs, Map<CopyBookFieldType,CopyBookFieldFormat> paddingDefaults) throws CopyBookException {
        this.line = copybookLine;
        this.charset = charset;
        this.fields = fields;
        this.counters = counters;
        this.indexes = indexes;
        this.occurs = occurs;

        // Parse copybook line and validate types
        Matcher matcher = re_pictype.matcher(copybookLine);
        Class fieldType = getField().getType();

        // Check if the field is also array
        if(isArray()) {
            if(fieldType.isArray()) {
                fieldType = fieldType.getComponentType();
            } else {
                throw new CopyBookException("Field '" + getFieldName() + "' is not an array type");
            }
        }

        if(matcher.find()) {
            if(matcher.group(1).equals("X")) { // String type
                if (fieldType.equals(String.class)) {
                    // String
                    this.type = CopyBookFieldType.STRING;
                    this.size = Integer.parseInt(matcher.group(2));
                    this.decimal = -1;
                } else {
                    throw new CopyBookException("Field " + getFieldName() + " is not one of the supported types(String) for this copybook line");
                }

            } else if (matcher.group(1).equals("S9")) { // Signed number
                    if (matcher.group(3) != null) {
                        // With decimals
                        if (fieldType.equals(Float.TYPE) || fieldType.equals(Double.TYPE) || fieldType.equals(BigDecimal.class)) {
                            this.type = CopyBookFieldType.SIGNED_DECIMAL;
                            this.size = Integer.parseInt(matcher.group(2));
                            this.decimal = Integer.parseInt(matcher.group(3));
                        } else {
                            throw new CopyBookException("Field " + getFieldName() + " is not one of the supported types(float, double, BigDecimal) for this copybook line");
                        }

                    } else {
                        if (fieldType.equals(Integer.TYPE) || fieldType.equals(Long.TYPE) || fieldType.equals(BigInteger.class)) {
                            // Without decimals
                            this.type = CopyBookFieldType.SIGNED_INT;
                            this.size = Integer.parseInt(matcher.group(2));
                            this.decimal = Integer.parseInt(matcher.group(3));
                        } else {
                            throw new CopyBookException("Field " + getFieldName() + " is not one of the supported types(int, long, BigInteger) for this copybook line");
                        }
                    }

            } else if (matcher.group(1).equals("9")) { // unsigned number
                // Check if it's a decimal number
                if (matcher.group(3) != null) {
                    // With decimals
                    if (fieldType.equals(Float.TYPE) || fieldType.equals(Double.TYPE) || fieldType.equals(BigDecimal.class)) {
                        this.type = CopyBookFieldType.DECIMAL;
                        this.size = Integer.parseInt(matcher.group(2));
                        this.decimal = -1;
                    } else {
                        throw new CopyBookException("Field " + getFieldName() + " is not one of the supported types(float, double, BigDecimal) for this copybook line");
                    }

                } else {
                    // Without decimals
                    if (fieldType.equals(Integer.TYPE) || fieldType.equals(Long.TYPE) || fieldType.equals(BigInteger.class)) {
                        this.type = CopyBookFieldType.INT;
                        this.size = Integer.parseInt(matcher.group(2));
                        this.decimal = -1;

                    } else {
                        throw new CopyBookException("Field " + getFieldName() + " is not one of the supported types(int, long, BigInteger) for this copybook line");
                    }
                }

            } else {
                throw new CopyBookException("Unknown PIC type");
            }

        } else {
            throw new CopyBookException("Could not parse the PIC type");
        }

        this.padding = (byte)paddingDefaults.get(this.type).paddingChar();
        this.rightPadding = paddingDefaults.get(this.type).rightPadding();
    }

    public Object get(Object obj) throws IllegalAccessException, CopyBookException {
        Object current = obj;
        for (int i = 0; i < fields.length; i++) {
            current = fields[i].get(current);
            // Handle array types
            if (current != null && indexes[i] > -1) {
                int current_length = Array.getLength(current);
                // Try to read the values in the array else return null
                if (indexes[i] < current_length) {
                    current = Array.get(current, indexes[i]);
                } else {
                    current = null;
                }

                // Validate that we can serialize this object
                if(current_length > occurs[i]) {
                    throw new CopyBookException("Array " + getFieldName(i) + " is larger than what is allowed by the copy book: " + occurs[i] + " < " + current_length);
                }
            }

            if (current == null) {
                break;
            }
        }

        return current;
    }

    public byte[] getBytes(Object obj, boolean addPadding) throws CopyBookException, IllegalAccessException {
        byte[] strBytes = new byte[0];

        // Get bytes for field
        Object current = get(obj);
        if(current != null) {
            switch (type) {
                case STRING: {
                    strBytes = ((String) current).getBytes(charset);
                    break;
                }
                case SIGNED_INT:
                case INT: {
                    strBytes = current.toString().getBytes(charset);
                    break;
                }
                case SIGNED_DECIMAL:
                case DECIMAL: {
                    strBytes = current.toString().getBytes(charset);
                    break;
                }
                default: {
                    throw new CopyBookException("Unknown copybook field type");
                }
            }
        }

        // Add padding to bytes
        if (strBytes.length <= size) {
            if (addPadding) {
                byte[] paddedStrBytes = new byte[size];
                Arrays.fill(paddedStrBytes, padding);
                if (rightPadding) {
                    System.arraycopy(strBytes, 0, paddedStrBytes, 0, strBytes.length);
                } else {
                    System.arraycopy(strBytes, 0, paddedStrBytes, paddedStrBytes.length - strBytes.length, strBytes.length);
                }
                return paddedStrBytes;

            } else if(strBytes.length > 0) {
                return strBytes;

            } else {
                return null;
            }

        } else {
            throw new CopyBookException("Field '" + getFieldName() + "' to long : " + strBytes.length + " > " + size);
        }
    }

    public void set(Object obj, byte[] value, boolean recursive, boolean trimPadding) throws IllegalAccessException, CopyBookException, InstantiationException {
        int[] sizes = new int[this.counters.length];
        set(obj, value, recursive, sizes, trimPadding);
    }

    public void set(Object obj, byte[] value, boolean recursive, int[] sizes, boolean trimPadding) throws CopyBookException {
        // Convert to native types
        try {
            Object result;
            Class fieldType = getField().getType();
            switch (type) {
                case STRING: {
                    result = new String(trimPadding ? ByteUtils.trim(value, padding, rightPadding, 0) : value, charset);
                    break;
                }
                case SIGNED_INT:
                case INT: {
                    String strValue = new String(trimPadding ? ByteUtils.trim(value, padding, rightPadding, 1) : value, charset);
                    if(fieldType.equals(Integer.TYPE)) {
                        result = Integer.parseInt(strValue);
                    } else if (fieldType.equals(Long.TYPE)) {
                        result = Long.parseLong(strValue);
                    } else if (fieldType.equals(BigInteger.class)) {
                        result = Long.parseLong(strValue);
                    } else {
                        throw new CopyBookException("Field did not match type : " + getFieldName());
                    }
                    break;
                }
                case SIGNED_DECIMAL:
                case DECIMAL: {
                    String strValue = new String(trimPadding ? ByteUtils.trim(value, padding, rightPadding, 1) : value, charset);
                    if(fieldType.equals(Float.TYPE)) {
                        result = Float.parseFloat(strValue);
                    } else if (fieldType.equals(Double.TYPE)) {
                        result = Double.parseDouble(strValue);
                    } else if (fieldType.equals(BigDecimal.class)) {
                        result = Double.parseDouble(strValue);
                    } else {
                        throw new CopyBookException("Field "+  getFieldName() + " type is not a supported for this copybook field type" );
                    }
                    break;
                }
                default: {
                    throw new CopyBookException("Unknown copybook field type");
                }
            }

            set(obj, result, recursive, sizes);

        } catch(NumberFormatException ex) {
            throw new CopyBookException("Failed to deserialize field '" + getFieldName() + "': ("+ ex.getClass().getName() + ") " + ex.getLocalizedMessage());
        }
    }

    public void set(Object obj, Object value, boolean recursive) throws CopyBookException {
        int[] sizes = new int[this.counters.length];
        set(obj, value, recursive, sizes);
    }

    public void set(Object obj, Object value, boolean recursive, int[] sizeHints) throws CopyBookException{
        Object current = obj;

        for (int i = 0; i < fields.length - 1; i++) {
            try {
                // Get existing object
                Object nextCurrent = fields[i].get(current);

                // Create new object to hold value
                if (nextCurrent == null) {
                    if (indexes[i] > -1) {
                        nextCurrent = Array.newInstance(fields[i].getType().getComponentType(), sizeHints[i] > -1 ? sizeHints[i] : this.occurs[i]);
                    } else {
                        try {
                            nextCurrent = fields[i].getType().newInstance();
                        } catch (InstantiationException e) {
                            throw new CopyBookException("Failed to create new object for field '" + getFieldName(i) + "': " + e.getLocalizedMessage());
                        }
                    }
                    fields[i].set(current, nextCurrent);
                }

                // Add new item to array
                if (indexes[i] > -1) {
                    Object arrayItem = Array.get(nextCurrent, indexes[i]);
                    if (arrayItem == null) {
                        arrayItem = fields[i].getType().getComponentType().newInstance();
                        if(Array.getLength(nextCurrent) > 0) {
                            Array.set(nextCurrent, indexes[i], arrayItem);
                        }
                    }
                    nextCurrent = arrayItem;
                }

                current = nextCurrent;
            } catch (IllegalAccessException e) {
                throw new CopyBookException("Failed to access object for field '" + getFieldName(i) + "': " + e.getLocalizedMessage());

            } catch (InstantiationException e) {
                throw new CopyBookException("Failed to create new array item for field '" + getFieldName(i) + "': " + e.getLocalizedMessage());
            }
        }

        try {
            Field field = fields[fields.length - 1];
            if (indexes[indexes.length - 1] > -1) {
                Object nextCurrent = field.get(current);
                if (nextCurrent == null) {
                    nextCurrent = Array.newInstance(field.getType().getComponentType(), sizeHints[sizeHints.length -1] > -1 ? sizeHints[sizeHints.length -1] : occurs[occurs.length -1]);
                    field.set(current, nextCurrent);
                }
                if(Array.getLength(nextCurrent) > 0) {
                    Array.set(nextCurrent, indexes[indexes.length - 1], value);
                }

            } else {
                field.set(current, value);
            }

        } catch (IllegalAccessException e) {
            throw new CopyBookException("Failed to access object for field '" + getFieldName(fields.length - 1) + "': " + e.getLocalizedMessage());
        }
    }

    public String getFieldName() {
        return getFieldName(fields.length - 1);
    }

    public String getFieldName(int index) {
        String result = this.fields[0].getName();
        for(int i=1; i <= index ; i++) {
            result += '.' + this.fields[i].getName();
        }
        return result;
    }

    public Field getField() {
        return fields[fields.length - 1];
    }

    public Field getField(int index) {
        return fields[index];
    }

    public void setCounter(CopyBookField copyBookField) {
        counters[counters.length -1] = copyBookField;
    }

    public void setCounter(int index, CopyBookField copyBookField) {
        counters[index] = copyBookField;
    }

    public int getIndex() {
        return indexes[indexes.length -1];
    }

    public int getIndex(int index) {
        return indexes[index];
    }

    public boolean isArray() {
        return indexes[indexes.length -1] > -1;
    }

    public boolean isArray(int index) {
        return indexes[index] > -1;
    }
}
