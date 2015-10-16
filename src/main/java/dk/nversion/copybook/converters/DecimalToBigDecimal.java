package dk.nversion.copybook.converters;

import dk.nversion.copybook.exceptions.TypeConverterException;

import java.math.BigDecimal;
import java.math.BigInteger;

public class DecimalToBigDecimal extends TypeConverterBase {
    @Override
    public void validate(Class type, int size, int decimals) throws TypeConverterException {
        if(!BigDecimal.class.equals(type)) {
            throw new TypeConverterException("Only supports converting to and from BigDecimal");
        }
    }

    @Override
    public Object to(byte[] bytes, int offset, int length, int decimals, boolean removePadding) throws TypeConverterException {
        return new BigDecimal(new BigInteger(getString(bytes, offset, length, removePadding, 1)), decimals);
    }

    @Override
    public byte[] from(Object value, int length, int decimals, boolean addPadding) throws TypeConverterException {
        BigDecimal i = ((BigDecimal)value);
        if(i.signum() == 1) {
            throw new TypeConverterException("Number can not be negative");
        }
        // TODO: Validate loss of precision
        byte[] strBytes = i.movePointRight(decimals).toBigInteger().abs().toString().getBytes(this.charset);
        if(addPadding) {
            strBytes = padBytes(strBytes, length);
        }
        return strBytes;
    }


}
