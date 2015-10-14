package dk.nversion.copybook;

import dk.nversion.ByteUtils;
import dk.nversion.copybook.annotations.CopyBookFieldFormat;
import dk.nversion.copybook.exceptions.CopyBookException;

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
    private Pattern re_pictype = Pattern.compile("^\\s*(\\d+)\\s+([^\\s]+)\\s+PIC\\s+(S)?(X+|9+)(?:\\((\\d+)\\))?(?:V(9+)(?:\\((\\d+)\\))?)?\\s*\\.\\s*$");

    public CopyBookFieldType type;
    public int offset;
    public int size;
    public int decimals;
    public Field[] fields;
    public int[] indexes;
    public int[] occurs;
    public int[] subFields;
    public CopyBookField[] counters;
    public String line;

    public boolean rightPadding;
    public byte padding;
    public byte nullFiller;
    public Charset charset;
    public CopyBookFieldSigningType signingType;

    public CopyBookField() {

    }

    public CopyBookField(String copybookLine, Charset charset, Field[] fields, CopyBookField[] counters, int[] indexes, int[] occurs, Map<CopyBookFieldType,CopyBookFieldFormat> paddingDefaults) throws CopyBookException {
        this.line = copybookLine;
        this.charset = charset;
        this.fields = fields;
        this.counters = counters;
        this.indexes = indexes;
        this.occurs = occurs;

        // Check if the field is also array and extract the component type for that
        Class fieldType = getField().getType();
        if(isArray()) {
            if(fieldType.isArray()) {
                fieldType = fieldType.getComponentType();
            } else {
                throw new CopyBookException("Field '" + getFieldName() + "' is not an array type");
            }
        }
        // Parse copybook line and validate types
        Matcher matcher = re_pictype.matcher(copybookLine);
        if(matcher.find()) {
            int level = Integer.parseInt(matcher.group(1));
            String name = matcher.group(2);
            boolean signed = matcher.group(3) != null ? true : false;
            String mainType = matcher.group(4);
            int mainSize = matcher.group(5) != null ? Integer.parseInt(matcher.group(5)) : mainType.length();
            String decimalType = matcher.group(6) != null ?  matcher.group(6) : "";
            int decimalSize = matcher.group(7) != null ? Integer.parseInt(matcher.group(7)) : decimalType.length();

            // Add decimals to size if they are set
            this.size = mainSize + decimalSize;
            this.decimals = decimalSize;

            // Find type for this copybook line
            if (mainType.startsWith("X")) { // String type
                if (fieldType.equals(String.class)) {
                    this.type = CopyBookFieldType.STRING;
                } else {
                    throw new CopyBookException("Field " + getFieldName() + " is not one of the supported types(String) for this copybook line");
                }

            } else if (mainType.startsWith("9")) { // Signed number
                if (!decimalType.isEmpty()) {
                    // With decimals
                    if (fieldType.equals(Float.TYPE) || fieldType.equals(Double.TYPE) || fieldType.equals(BigDecimal.class)) {
                        this.type = signed ? CopyBookFieldType.SIGNED_DECIMAL : CopyBookFieldType.DECIMAL;
                    } else {
                        throw new CopyBookException("Field " + getFieldName() + " is not one of the supported types(float, double, BigDecimal) for this copybook line");
                    }

                } else {
                    // Without decimals
                    if (fieldType.equals(Integer.TYPE) || fieldType.equals(Long.TYPE) || fieldType.equals(BigInteger.class)) {
                        this.type = signed ? CopyBookFieldType.SIGNED_INT : CopyBookFieldType.INT;
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

        byte[] paddingBytes = (paddingDefaults.get(this.type).paddingChar() + "").getBytes(charset);
        if(paddingBytes.length > 1) {
            throw new CopyBookException("Selected charset and padding char is more than 1 byte long for field '"+ getFieldName() + "'");
        }
        this.padding = paddingBytes[0];

        byte[] nullFillerBytes = (paddingDefaults.get(this.type).nullFillerChar() + "").getBytes(charset);
        if(nullFillerBytes.length > 1) {
            throw new CopyBookException("Selected charset and null filler char is more than 1 byte long for field '"+ getFieldName() + "'");
        }
        this.nullFiller = nullFillerBytes[0];

        this.rightPadding = paddingDefaults.get(this.type).rightPadding();
        this.signingType = paddingDefaults.get(this.type).signingType();
    }

    public Object get(Object obj) throws IllegalAccessException, CopyBookException {
        return get(obj, fields.length - 1);
    }

    public Object get(Object obj, int index) throws IllegalAccessException, CopyBookException {
        Object current = obj;
        for (int i = 0; i <= index; i++) {
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

    public byte[] getBytes(Object current, boolean addPaddingOrNullFiller) throws CopyBookException, IllegalAccessException {
        byte[] strBytes = new byte[0];

        // Get bytes for field
        if(current != null) {
            String valueString;
            String signString = null;
            switch (type) {
                case STRING: {
                    valueString = ((String) current);
                    break;
                }
                case SIGNED_INT:
                case INT: {
                    if(Integer.class.isInstance(current)) {
                        Integer value = ((Integer)current);
                        valueString = String.valueOf(Math.abs(value.intValue()));
                        if(type == CopyBookFieldType.SIGNED_INT) {
                            signString = value.intValue() < 0 ? "-" : "+";
                        } else if (value.intValue() < 0) {
                            throw new CopyBookException("Unsigned field '" + getFieldName() + "' has negative value");
                        }

                    } else if(Long.class.isInstance(current)) {
                        Long value = ((Long)current);
                        valueString = String.valueOf(Math.abs(value.longValue()));
                        if(type == CopyBookFieldType.SIGNED_INT) {
                            signString = value.longValue() < 0L ? "-" : "+";
                        } else if (value.longValue() < 0L) {
                            throw new CopyBookException("Unsigned field '" + getFieldName() + "' has negative value");
                        }

                    } else if(BigInteger.class.isInstance(current)) {
                        BigInteger value = ((BigInteger)current);
                        valueString = String.valueOf(value.abs());
                        if(type == CopyBookFieldType.SIGNED_INT) {
                            signString = value.signum() < 0 ? "-" : "+";
                        } else if (value.signum() < 0) {
                            throw new CopyBookException("Unsigned field '" + getFieldName() + "' has negative value");
                        }

                    } else {
                        throw new CopyBookException("Unsupported field type");
                    }
                    break;
                }
                case SIGNED_DECIMAL:
                case DECIMAL: {
                    if(BigDecimal.class.isInstance(current)) {
                        BigDecimal value = ((BigDecimal)current);
                        valueString = value.movePointRight(2).toBigInteger().abs().toString();
                        if(type == CopyBookFieldType.SIGNED_DECIMAL) {
                            signString = value.signum() < 0 ? "-" : "+";
                        } else if (value.signum() < 0) {
                            throw new CopyBookException("Unsigned field '" + getFieldName() + "' has negative value");
                        }

                    } else if(Float.class.isInstance(current)) {
                        throw new CopyBookException("Not implement yet"); // TODO: Implement Float support
                    } else if(Double.class.isInstance(current)) {
                        throw new CopyBookException("Not implement yet"); // TODO: Implement Double support
                    } else {
                        throw new CopyBookException("Unsupported field type");
                    }
                    break;
                }
                default: {
                    throw new CopyBookException("Unknown copybook field type");
                }
            }

            if(signString != null) {
                if (signingType == CopyBookFieldSigningType.POSTFIX) {
                    strBytes = (valueString + signString).getBytes(charset);
                } else if (signingType == CopyBookFieldSigningType.PREFIX) {
                    strBytes = (signString + valueString).getBytes(charset);
                } else if (signingType == CopyBookFieldSigningType.LAST_BYTE_BIT8) {
                    strBytes = valueString.getBytes(charset);
                    if(signString.equals("-")) {
                        strBytes[strBytes.length -1] = (byte)(strBytes[strBytes.length -1] | 128); // Set bit 8
                    } else {
                        strBytes[strBytes.length -1] = (byte)(strBytes[strBytes.length -1] | 127); // Unset bit 8
                    }

                } else if (signingType == CopyBookFieldSigningType.LAST_BYTE_EBCDIC_BIT5) {
                    strBytes = valueString.getBytes(charset);
                    strBytes[strBytes.length -1] = (byte)(strBytes[strBytes.length -1] & 15); // zero top 4 bits, 00001111
                    if(signString.equals("-")) {
                        strBytes[strBytes.length -1] = (byte)(strBytes[strBytes.length -1] | 208); // Set bits 1101 0000
                    } else {
                        strBytes[strBytes.length -1] = (byte)(strBytes[strBytes.length -1] | 192); // Set bits 1100 0000
                    }
                }

            } else {
                strBytes = valueString.getBytes(charset);
            }

        } else if(addPaddingOrNullFiller) {
            strBytes = new byte[size];
            Arrays.fill(strBytes, nullFiller);
        }

        // Add padding to bytes
        if (strBytes.length <= size) {
            if (addPaddingOrNullFiller) {
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
            throw new CopyBookException("Field '" + getFieldName() + "' serialized bytes are to long : " + strBytes.length + " > " + size);
        }
    }

    public void set(Object obj, byte[] value, boolean recursive, boolean trimPadding) throws IllegalAccessException, CopyBookException, InstantiationException {
        int[] sizeHints = new int[this.counters.length];
        set(obj, value, recursive, trimPadding, sizeHints);
    }

    public void set(Object obj, byte[] value, boolean recursive, boolean trimPadding, int[] sizeHints) throws CopyBookException {
        // Convert to native types
        try {
            Object result;
            Class fieldType = getField().getType();
            if(fieldType.isArray()) {
                fieldType = fieldType.getComponentType();
            }

            if(type == CopyBookFieldType.STRING) {
                if(ByteUtils.allEquals(value, nullFiller, 0, value.length)) { // All of value is null filler
                    result = null;
                } else {
                    result = new String(trimPadding ? ByteUtils.trim(value, padding, rightPadding, 0) : value, charset);
                }

            } else {
                String strValue;

                // TODO: Refactor and move to own function
                // Fix signing
                if (type == CopyBookFieldType.SIGNED_INT || type == CopyBookFieldType.SIGNED_DECIMAL) {
                    if(signingType == CopyBookFieldSigningType.POSTFIX) {
                        strValue = normalizeNumericSigning(new String(trimPadding ? ByteUtils.trim(value, padding, rightPadding, 1) : value, charset), true);

                    } else if (signingType == CopyBookFieldSigningType.PREFIX) {
                        strValue = normalizeNumericSigning(new String(trimPadding ? ByteUtils.trim(value, padding, rightPadding, 1) : value, charset), false);

                    } else if (signingType == CopyBookFieldSigningType.LAST_BYTE_BIT8) {
                        if((value[value.length -1] & 128) != 0) { // Check if bit 8 is set
                            byte[] valueCopy = Arrays.copyOf(value, value.length);
                            valueCopy[valueCopy.length -1] =  (byte)(valueCopy[valueCopy.length -1] & 127);
                            strValue = "-" + new String(trimPadding ? ByteUtils.trim(valueCopy, padding, rightPadding, 1) : valueCopy, charset);

                        } else {
                            strValue = new String(trimPadding ? ByteUtils.trim(value, padding, rightPadding, 1) : value, charset);
                        }
                    } else if (signingType == CopyBookFieldSigningType.LAST_BYTE_EBCDIC_BIT5) {
                        byte res = (byte)(value[value.length -1] & 240); // Read last byte and zero first 4 bits of the result, 11110000
                        byte[] valueCopy = Arrays.copyOf(value, value.length - 1);
                        if((byte)(res ^ 208) == 0 ||(byte)(res ^ 176) == 0) { // 208 = 11010000, 176 = 10110000
                            strValue = "-" + new String(trimPadding ? ByteUtils.trim(valueCopy, padding, rightPadding, 1) : valueCopy, charset) + String.valueOf((byte)value[value.length -1] & 15);
                        } else {
                            strValue = new String(trimPadding ? ByteUtils.trim(valueCopy, padding, rightPadding, 1) : valueCopy, charset) + String.valueOf((byte)value[value.length -1] & 15);
                        }

                    } else {
                        throw new CopyBookException("Unknown signing type for field '" + getFieldName()+ "'");
                    }

                } else {
                    strValue = new String(trimPadding ? ByteUtils.trim(value, padding, rightPadding, 1) : value, charset);
                    if(strValue.startsWith("-") || strValue.endsWith("-")) {
                        throw new CopyBookException("Unsigned field '" + getFieldName()+ "' starts or ends with -");
                    }
                }

                if(type == CopyBookFieldType.INT || type == CopyBookFieldType.SIGNED_INT) {
                    // Parse numeric
                    if(fieldType.equals(Integer.TYPE)) {
                        result = Integer.parseInt(strValue);
                    } else if (fieldType.equals(Long.TYPE)) {
                        result = Long.parseLong(strValue);
                    } else if (fieldType.equals(BigInteger.class)) {
                        result = new BigInteger(strValue);
                    } else {
                        throw new CopyBookException("Field '" + getFieldName() + "' did not match expected type");
                    }

                } else if(type == CopyBookFieldType.DECIMAL || type == CopyBookFieldType.SIGNED_DECIMAL) {
                    // Parse numeric
                    if(fieldType.equals(Float.TYPE)) {
                        throw new CopyBookException("Not implement yet"); // TODO: Implement Float support
                    } else if (fieldType.equals(Double.TYPE)) {
                        throw new CopyBookException("Not implement yet"); // TODO: Implement Double support
                    } else if (fieldType.equals(BigDecimal.class)) {
                        result = new BigDecimal(new BigInteger(strValue), decimals);
                    } else {
                        throw new CopyBookException("The type for field '" + getFieldName() + "' is not a supported for this copybook field type" );
                    }
                } else {
                    throw new CopyBookException("Unknown copybook field type");
                }
            }

            set(obj, result, recursive, sizeHints);

        } catch(NumberFormatException ex) {
            throw new CopyBookException("Failed to deserialize field '" + getFieldName() + "': ("+ ex.getClass().getName() + ") " + ex.getLocalizedMessage());
        }
    }

    private String normalizeNumericSigning(String str, boolean signingPostfix) throws CopyBookException {
        if (signingPostfix && str.endsWith("-")) {
            str = '-' + str.substring(0, str.length() - 1);
        } else if (signingPostfix && str.endsWith("+")) {
            str = str.substring(0, str.length() - 1);
        } else if (str.startsWith("+")) {
            str = str.substring(1, str.length());
        } else if (str.startsWith("-")) {
            // DO nothing
        } else {
            throw new CopyBookException("Field '"+ getFieldName() + "' is missing signed chars for value '" + str + "'");
        }
        return str;
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

    public boolean isNull(Object obj) throws CopyBookException, IllegalAccessException {
        return get(obj) == null;
    }

    public boolean isNull(Object obj, int index) throws CopyBookException, IllegalAccessException {
        return get(obj, index) == null;
    }
}
