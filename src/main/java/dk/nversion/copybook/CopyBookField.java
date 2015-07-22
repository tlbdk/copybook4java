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
    private Pattern re_pictype = Pattern.compile("(X|9|S9)\\((\\d+)\\)(?:V9\\((\\d+)\\))?");

    public CopyBookFieldType type;
    public int offset;
    public int size;
    public int decimal;
    public Field[] fields;
    public int[] indexs;
    public int[] occurs;
    public CopyBookField[] counters;
    public String line;

    public boolean rightpadding;
    public byte padding;
    public Charset charset;

    public boolean packingItem;

    public CopyBookField() {

    }

    public CopyBookField(String copybookline, Map<CopyBookFieldType,CopyBookFieldFormat> paddingDefaults) throws Exception {
        this.line = copybookline;
        Matcher matcher = re_pictype.matcher(copybookline);
        if(matcher.find()) {
            if(matcher.group(1).equals("X")) { // String type
                // String
                this.type = CopyBookFieldType.STRING;
                this.size = Integer.parseInt(matcher.group(2));
                this.decimal = -1;

            } else if (matcher.group(1).equals("S9")) { // Signed number
                if(matcher.group(3) != null) {
                    // With decimals
                    this.type = CopyBookFieldType.SIGNED_INT;
                    this.size = Integer.parseInt(matcher.group(2));
                    this.decimal = Integer.parseInt(matcher.group(3));

                } else {
                    // Without decimals
                    this.type = CopyBookFieldType.DECIMAL;
                    this.size = Integer.parseInt(matcher.group(2));
                    this.decimal = Integer.parseInt(matcher.group(3));
                }

            } else if (matcher.group(1).equals("9")) { // unsigned number
                // Check if it's a decimal number
                if(matcher.group(3) != null) {
                    // With decimals
                    this.type = CopyBookFieldType.DECIMAL;
                    this.size = Integer.parseInt(matcher.group(2));
                    this.decimal = -1;

                } else {
                    // Without decimals
                    this.type = CopyBookFieldType.INT;
                    this.size = Integer.parseInt(matcher.group(2));
                    this.decimal = -1;
                }
            } else {
                throw new Exception("Unknown PIC type");
            }

        } else {
            throw new Exception("Could not find any PIC type");
        }

        this.padding = (byte)paddingDefaults.get(this.type).paddingChar();
        this.rightpadding = paddingDefaults.get(this.type).rightPadding();
    }

    public void setOccurs() {

    }

    public CopyBookField(CopyBookFieldType type, int size, int decimal, Field[] fields, int[] indexs) {
        this.type = type;
        this.size = size;
        this.decimal = decimal;
        this.fields = fields;
        this.indexs = indexs;
    }

    public Object get(Object obj) throws IllegalAccessException, CopyBookException {
        Object current = obj;
        for (int i = 0; i < fields.length; i++) {
            current = fields[i].get(current);
            // Handle array types
            if (current != null && indexs[i] > -1) {
                int current_length = Array.getLength(current);
                // Try to read the values in the array else return null
                if (indexs[i] < current_length) {
                    current = Array.get(current, indexs[i]);
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
        // FIXME: Return string if addPadding is set and get returns null
        Object current = get(obj);
        if(current != null) {
            byte[] strBytes;
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

            byte[] result = strBytes;

            // Add padding to bytes
            if (strBytes.length <= size) {
                if (addPadding) {
                    result = new byte[size];
                    Arrays.fill(result, padding);
                    if (rightpadding) {
                        System.arraycopy(strBytes, 0, result, 0, strBytes.length);
                    } else {
                        System.arraycopy(strBytes, 0, result, result.length - strBytes.length, strBytes.length);
                    }
                }

            } else {
                throw new CopyBookException("Field '" + getFieldName() + "' to long : " + strBytes.length + " > " + size);
            }

            return result;

        } else {
            return null;
        }
    }

    public void set(Object obj, byte[] value, boolean recursive) throws IllegalAccessException, CopyBookException, InstantiationException {
        int[] sizes = new int[this.counters.length];
        set(obj, value, recursive, sizes);
    }

    public void set(Object obj, byte[] value, boolean recursive, int[] sizes) throws CopyBookException {
        // TODO: Check if padding is enabled for this field
        // Convert field bytes to string and trim value
        String strvalue = new String(ByteUtils.trim(value, padding, rightpadding, 1), charset);

        // Convert to native types
        try {
            Object result;
            Class fieldtype = getLastField().getType();
            switch (type) {
                case STRING: {
                    result = strvalue;
                    break;
                }
                case SIGNED_INT:
                case INT: {
                    if(fieldtype.equals(Integer.TYPE)) {
                        result = Integer.parseInt(strvalue);
                    } else if (fieldtype.equals(Long.TYPE)) {
                        result = Long.parseLong(strvalue);
                    } else if (fieldtype.equals(BigInteger.class)) {
                        result = Long.parseLong(strvalue);
                    } else {
                        throw new CopyBookException("Field did not match type : " + getFieldName());
                    }
                    break;
                }
                case SIGNED_DECIMAL:
                case DECIMAL: {
                    if(fieldtype.equals(Float.TYPE)) {
                        result = Float.parseFloat(strvalue);
                    } else if (fieldtype.equals(Double.TYPE)) {
                        result = Double.parseDouble(strvalue);
                    } else if (fieldtype.equals(BigDecimal.class)) {
                        result = Double.parseDouble(strvalue);
                    } else {
                        throw new CopyBookException("Field did not match type : " + getFieldName());
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
                    if (indexs[i] > -1) {
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

                if (indexs[i] > -1) {
                    Object arrayItem = Array.get(nextCurrent, indexs[i]);
                    if (arrayItem == null) {
                        arrayItem = fields[i].getType().getComponentType().newInstance();
                        if(Array.getLength(nextCurrent) > 0) {
                            Array.set(nextCurrent, indexs[i], arrayItem);
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
            if (indexs[indexs.length - 1] > -1) {
                Object nextCurrent = field.get(current);
                if (nextCurrent == null) {
                    nextCurrent = Array.newInstance(field.getType().getComponentType(), sizeHints[sizeHints.length -1] > -1 ? sizeHints[sizeHints.length -1] : occurs[occurs.length -1]);
                    field.set(current, nextCurrent);
                }
                if(Array.getLength(nextCurrent) > 0) {
                    Array.set(nextCurrent, indexs[indexs.length - 1], value);
                }

            } else {
                field.set(current, value);
            }

        } catch (IllegalAccessException e) {
            throw new CopyBookException("Failed to access object for field '" + getFieldName(fields.length - 1) + "': " + e.getLocalizedMessage());
        }
    }

    public Class getFieldType() {
        return fields[fields.length - 1].getType();
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

    public Field getLastField() {
        return fields[fields.length - 1];
    }

    public int getIndex() {
        return indexs[indexs.length -1];
    }

    public int getIndex(int index) {
        return indexs[index];
    }

    public boolean isArray() {
        return indexs[indexs.length -1] > -1;
    }

    public boolean isArray(int index) {
        return indexs[index] > -1;
    }

    public Object newInstanceLastField() throws IllegalAccessException, InstantiationException {
        return newInstance(fields.length - 1);
    }

    public Object newInstance(int index) throws IllegalAccessException, InstantiationException {
        if(isArray()) {
            Object obj = Array.newInstance(this.fields[index].getType().getComponentType(), this.occurs[index]);
            for(int i=0; i < this.occurs[index]; i++) {
                Array.set(obj, i, this.fields[index].getType().getComponentType().newInstance());
            }
            return obj;

        } else {
            return this.fields[index].getType().newInstance();
        }

    }
}
