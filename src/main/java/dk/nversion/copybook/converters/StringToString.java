package dk.nversion.copybook.converters;

import dk.nversion.ByteUtils;
import dk.nversion.copybook.exceptions.TypeConverterException;

public class StringToString extends TypeConverterBase {
    @Override
    public void validate(Class type, int size, int decimals) throws TypeConverterException {
        if(!(String.class.equals(type))) {
            throw new TypeConverterException("Only supports converting to and from String");
        }
    }

    @Override
    public String to(byte[] bytes, int offset, int length, int decimals, boolean removePadding) throws TypeConverterException {
        if(ByteUtils.allEquals(bytes, (byte)this.nullFillerChar, 0, bytes.length)) { // All of value is null filler
            return null;

        } else {
            return getString(bytes, offset, length, removePadding, 0);
        }
    }

    @Override
    public byte[] from(Object value, int length, int decimals, boolean addPadding) throws TypeConverterException {
        byte[] strBytes = ((String)value).getBytes();
        if(addPadding) {
            strBytes = padBytes(strBytes, length);
        }
        return strBytes;
    }
}
