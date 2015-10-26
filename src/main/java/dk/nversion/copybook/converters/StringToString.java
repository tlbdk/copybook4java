package dk.nversion.copybook.converters;

import dk.nversion.ByteUtils;

import java.util.Arrays;

public class StringToString extends TypeConverterBase {
    @Override
    public void validate(Class type, int size, int decimals) throws TypeConverterException {
        if(!(String.class.equals(type))) {
            throw new TypeConverterException("Only supports converting to and from String");
        }
    }

    @Override
    public String to(byte[] bytes, int offset, int length, int decimals, boolean removePadding) throws TypeConverterException {
        if(ByteUtils.allEquals(bytes, this.nullFillerByte, offset, bytes.length)) { // All of value is null filler
            if(this.defaultValue != null) {
                return this.defaultValue;

            } else {
                return null;
            }

        } else {
            return getString(bytes, offset, length, removePadding, 0);
        }
    }

    @Override
    public byte[] from(Object value, int length, int decimals, boolean addPadding) throws TypeConverterException {
        byte[] strBytes = null;
        if(value != null || this.defaultValue != null) {
            strBytes = (value != null ? (String)value : this.defaultValue).getBytes(this.charset);
            if (strBytes.length > length) {
                throw new TypeConverterException("Field to small for value: " + length + " < " + strBytes.length);
            }
            if (addPadding) {
                strBytes = padBytes(strBytes, length);
            }

        } else if(addPadding) {
            strBytes = new byte[length];
            Arrays.fill(strBytes, this.nullFillerByte);
        }

        return strBytes;
    }
}
