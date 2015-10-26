package dk.nversion.copybook.converters;

import dk.nversion.ByteUtils;
import dk.nversion.copybook.serializers.CopyBookFieldSigningType;

public class SignedIntegerToLong extends SignedIntegerToInteger {
    @Override
    public void validate(Class type, int size, int decimals) throws TypeConverterException {
        if(size > 22 && (this.signingType == CopyBookFieldSigningType.PREFIX || this.signingType == CopyBookFieldSigningType.POSTFIX)) {
            throw new TypeConverterException("long is not large enough to hold possible value");
        }
        if(size > 21 && (this.signingType == CopyBookFieldSigningType.LAST_BYTE_BIT8 || this.signingType == CopyBookFieldSigningType.LAST_BYTE_EBCDIC_BIT5)) {
            throw new TypeConverterException("long is not large enough to hold possible value");
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
            return Long.parseLong(getSignedIntegerString(bytes, offset, length, removePadding));
        }
    }

    @Override
    public byte[] from(Object value, int length, int decimals, boolean addPadding) throws TypeConverterException {
        long i = (long)value;
        byte[] strBytes = getSignedBytes(Long.toString(Math.abs(i)), i < 0L);
        if(strBytes.length > length) {
            throw new TypeConverterException("Field to small for value: " + length + " < " + strBytes.length);
        }
        if(addPadding) {
            strBytes = padBytes(strBytes, length);
        }
        return strBytes;
    }
}
