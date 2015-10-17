package dk.nversion.copybook.converters;

import dk.nversion.copybook.exceptions.TypeConverterException;

import java.math.BigInteger;

public class SignedIntegerToBigInteger extends SignedIntegerToInteger {
    @Override
    public void validate(Class type, int size, int decimals) throws TypeConverterException {
        if(!BigInteger.class.equals(type)) {
            throw new TypeConverterException("Only supports converting to and from BigInteger");
        }
    }

    @Override
    public Object to(byte[] bytes, int offset, int length, int decimals, boolean removePadding) throws TypeConverterException {
        return new BigInteger(getSignedIntegerString(bytes, offset, length, removePadding));
    }

    @Override
    public byte[] from(Object value, int length, int decimals, boolean addPadding) throws TypeConverterException {
        BigInteger i = value != null ? (BigInteger)value : new BigInteger("0");
        byte[] strBytes = getSignedBytes(i.abs().toString(), i.signum() < 0);
        if(strBytes.length > length) {
            throw new TypeConverterException("Field to small for value: " + length + " < " + strBytes.length);
        }
        if(addPadding) {
            strBytes = padBytes(strBytes, length);
        }
        return strBytes;

    }
}
