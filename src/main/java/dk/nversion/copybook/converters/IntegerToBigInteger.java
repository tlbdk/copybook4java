package dk.nversion.copybook.converters;

import dk.nversion.ByteUtils;
import dk.nversion.copybook.exceptions.TypeConverterException;

import java.math.BigInteger;

public class IntegerToBigInteger extends IntegerToInteger {
    @Override
    public void validate(Class type, int size, int decimals) throws TypeConverterException {
        if(!BigInteger.class.isInstance(type)) {
            throw new TypeConverterException("Only supports converting to and from BigInteger");
        }
    }

    @Override
    public BigInteger to(byte[] bytes, int offset, int length, boolean removePadding) throws TypeConverterException {
        return new BigInteger(getIntegerString(bytes, offset, length, removePadding));
    }

    @Override
    public byte[] from(Object value, int length, boolean addPadding) throws TypeConverterException {
        BigInteger i = (BigInteger)value;
        if(i.signum() == 1) {
            throw new TypeConverterException("Number can not be negative");
        }
        byte[] strBytes = i.abs().toString().getBytes(this.charset);

        if(addPadding) {
            strBytes = padBytes(strBytes, length);
        }
        return strBytes;
    }
}
