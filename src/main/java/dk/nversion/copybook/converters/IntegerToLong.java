package dk.nversion.copybook.converters;

import dk.nversion.ByteUtils;
import dk.nversion.copybook.exceptions.TypeConverterException;

public class IntegerToLong extends IntegerToInteger {
    @Override
    public void validate(Class type, int size, int decimals) throws TypeConverterException {
        if(size > 21) {
            throw new TypeConverterException("Long is not large enough to hold possible value");
        }
        if(!(Long.class.isInstance(type) || Long.TYPE.equals(type))) {
            throw new TypeConverterException("Only supports converting to and from long or Long");
        }
    }

    @Override
    public Object to(byte[] bytes, int offset, int length, boolean removePadding) throws TypeConverterException {
        return Long.parseLong(getIntegerString(bytes, offset, length, removePadding));
    }

    @Override
    public byte[] from(Object value, int length, boolean addPadding) throws TypeConverterException {
        long i = (long)value;
        if(i < 0) {
            throw new TypeConverterException("Number can not be negative");
        }
        byte[] strBytes = Long.toString(i).getBytes(this.charset);
        if(addPadding) {
            strBytes = padBytes(strBytes, length);
        }
        return strBytes;
    }
}
