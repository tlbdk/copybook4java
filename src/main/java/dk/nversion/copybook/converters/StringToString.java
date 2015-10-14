package dk.nversion.copybook.converters;

import dk.nversion.ByteUtils;
import dk.nversion.copybook.exceptions.TypeConverterException;

public class StringToString extends TypeConverterBase {
    @Override
    public void validate(Class type, int size, int decimals) throws TypeConverterException {

    }

    @Override
    public String to(byte[] bytes, int offset, int length, boolean removePadding) {
        if(ByteUtils.allEquals(bytes, (byte)this.nullFillerChar, 0, bytes.length)) { // All of value is null filler
            return null;
        } else {
            return new String(removePadding ? ByteUtils.trim(bytes, (byte)this.paddingChar, this.rightPadding, 0) : bytes, this.charset);
        }
    }

    @Override
    public byte[] from(Object value, int length, boolean addPadding) throws TypeConverterException {
        return new byte[0];
    }
}
