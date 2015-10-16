package dk.nversion.copybook.converters;

import dk.nversion.copybook.exceptions.TypeConverterException;

public class IntegerToInteger extends TypeConverterBase {

    public void validate(Class type, int size, int decimal) throws TypeConverterException {
        if(size > 9) {
            throw new TypeConverterException("int is not large enough to hold possible value");
        }
        if(!(Integer.class.equals(type) || Integer.TYPE.equals(type))) {
            throw new TypeConverterException("Only supports converting to and from int or Integer");
        }
    }

    public Object to(byte[] bytes, int offset, int length, int decimals, boolean removePadding) throws TypeConverterException {
        return Integer.parseInt(getIntegerString(bytes, offset, length, removePadding));
    }

    public byte[] from(Object value, int length, int decimals, boolean addPadding) throws TypeConverterException {
        int i = (int)value;
        if(i < 0) {
            throw new TypeConverterException("Number can not be negative");
        }
        byte[] strBytes = Integer.toString(i).getBytes(this.charset);
        if(addPadding) {
            strBytes = padBytes(strBytes, length);
        }
        return strBytes;
    }

    protected String getIntegerString(byte[] bytes, int offset, int length, boolean removePadding) throws TypeConverterException {
        String strValue = getString(bytes, offset, length, removePadding, 1);
        if(strValue.startsWith("-") || strValue.endsWith("-")) {
            throw new TypeConverterException("Integer value can not start or end with -");
        }
        return strValue;
    }
}
