package dk.nversion.copybook.converters;

import dk.nversion.ByteUtils;

public class IntegerToLong extends IntegerToInteger {
    @Override
    public void validate(Class type, int size, int decimals) throws TypeConverterException {
        if(size > 21) {
            throw new TypeConverterException("Long is not large enough to hold possible value");

        }
        if(!(Long.class.equals(type) || Long.TYPE.equals(type))) {
            throw new TypeConverterException("Only supports converting to and from long or Long");
        }
    }

    @Override
    public Object to(byte[] bytes, int offset, int length, int decimals, boolean removePadding) throws TypeConverterException {
        if(this.defaultValue != null && ByteUtils.allEquals(bytes, this.nullFillerByte, offset, bytes.length)) { // All of value is null filler
            return Long.parseLong(defaultValue);
        } else {
            return Long.parseLong(getIntegerString(bytes, offset, length, removePadding));
        }
    }

    @Override
    public byte[] from(Object value, int length, int decimals, boolean addPadding) throws TypeConverterException {
        long i = (long)value;
        if(i < 0) {
            throw new TypeConverterException("Number can not be negative");
        }

        byte[] strBytes = Long.toString(i).getBytes(this.charset);
        if(strBytes.length > length) {
            throw new TypeConverterException("Field to small for value: " + length + " < " + strBytes.length);
        }
        if(addPadding) {
            strBytes = padBytes(strBytes, length);
        }
        return strBytes;
    }
}
