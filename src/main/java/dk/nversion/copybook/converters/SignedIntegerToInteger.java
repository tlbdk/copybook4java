package dk.nversion.copybook.converters;

import dk.nversion.ByteUtils;
import dk.nversion.copybook.serializers.CopyBookFieldSigningType;

import java.util.Arrays;

public class SignedIntegerToInteger extends TypeConverterBase {
    @Override
    public void validate(Class<?> type, int size, int decimals) throws TypeConverterException {
        if(size > 10 && (this.signingType == CopyBookFieldSigningType.PREFIX || this.signingType == CopyBookFieldSigningType.POSTFIX)) {
            throw new TypeConverterException("int is not large enough to hold possible value");
        }
        if(size > 9 && (this.signingType == CopyBookFieldSigningType.LAST_BYTE_BIT8 || this.signingType == CopyBookFieldSigningType.LAST_BYTE_EBCDIC_BIT5)) {
            throw new TypeConverterException("int is not large enough to hold possible value");
        }

        if(!(Integer.class.equals(type) || Integer.TYPE.equals(type))) {
            throw new TypeConverterException("Only supports converting to and from int or Integer");
        }
    }

    @Override
    public Object to(byte[] bytes, int offset, int length, int decimals, boolean removePadding) throws TypeConverterException {
        if(this.defaultValue != null && ByteUtils.allEquals(bytes, this.nullFillerByte, offset, bytes.length)) { // All of value is null filler
            return Integer.parseInt(defaultValue);

        } else {
            return Integer.parseInt(getSignedIntegerString(bytes, offset, length, removePadding));
        }
    }

    @Override
    public byte[] from(Object value, int length, int decimals, boolean addPadding) throws TypeConverterException {
        if(value == null && this.defaultValue == null) {
            return null;
        }

        int i = value != null ? (int) value : Integer.parseInt(this.defaultValue);
        byte[] strBytes = getSignedBytes(Integer.toString(Math.abs(i)), i < 0);
        if(strBytes.length > length) {
            throw new TypeConverterException("Field to small for value: " + length + " < " + strBytes.length);
        }
        if(addPadding) {
            strBytes = padBytes(strBytes, length);
        }
        return strBytes;
    }

    protected String getSignedIntegerString(byte[] bytes, int offset, int length, boolean removePadding) throws TypeConverterException {
        String strValue;

        if(this.signingType == CopyBookFieldSigningType.POSTFIX) {
            strValue = normalizeNumericSigning(getString(bytes, offset, length, removePadding, 2), true);

        } else if(this.signingType == CopyBookFieldSigningType.PREFIX) {
            strValue = normalizeNumericSigning(getString(bytes, offset, length, removePadding, 2), false);

        } else if (signingType == CopyBookFieldSigningType.LAST_BYTE_BIT8) {
            if ((bytes[bytes.length - 1] & 128) != 0) { // Check if bit 8 is set
                byte[] bytesCopy = Arrays.copyOf(bytes, bytes.length);
                bytesCopy[bytesCopy.length - 1] = (byte) (bytesCopy[bytesCopy.length - 1] & 127);
                strValue = "-" + getString(bytesCopy, 0, bytesCopy.length, removePadding, 1);

            } else {
                strValue = getString(bytes, offset, length, removePadding, 1);;
            }

        } else if (signingType == CopyBookFieldSigningType.LAST_BYTE_EBCDIC_BIT5) {
            byte res = (byte)(bytes[bytes.length -1] & 240); // Read last byte and zero first 4 bits of the result, 11110000
            byte[] bytesCopy = Arrays.copyOf(bytes, bytes.length - 1);
            if((byte)(res ^ 208) == 0 ||(byte)(res ^ 176) == 0) { // 208 = 11010000, 176 = 10110000
                strValue = "-" + getString(bytesCopy, offset, length -1, removePadding, 1) + String.valueOf(bytes[bytes.length -1] & 15);
            } else {
                strValue = getString(bytesCopy, offset, bytesCopy.length, removePadding, 1) + String.valueOf(bytes[bytes.length -1] & 15);
            }

        } else {
            throw new TypeConverterException("Unknown signing type");
        }

        return strValue;
    }

    protected byte[] getSignedBytes(String strValue, boolean negative) throws TypeConverterException {
        byte[] strBytes;

        if (this.signingType == CopyBookFieldSigningType.POSTFIX) {
            strBytes = (strValue + (negative ? '-' : "+")).getBytes(this.charset);

        } else if (signingType == CopyBookFieldSigningType.PREFIX) {
            strBytes = ((negative ? '-' : "+") + strValue).getBytes(this.charset);

        } else if (signingType == CopyBookFieldSigningType.LAST_BYTE_BIT8) {
            strBytes = strValue.getBytes(charset);
            if(negative) {
                strBytes[strBytes.length -1] = (byte)(strBytes[strBytes.length -1] | 128); // Set bit 8

            } else {
                strBytes[strBytes.length -1] = (byte)(strBytes[strBytes.length -1] & 127); // Unset bit 8
            }

        } else if (signingType == CopyBookFieldSigningType.LAST_BYTE_EBCDIC_BIT5) {
            strBytes = strValue.getBytes(charset);
            strBytes[strBytes.length -1] = (byte)(strBytes[strBytes.length -1] & 15); // zero top 4 bits, 00001111
            if(negative) {
                strBytes[strBytes.length -1] = (byte)(strBytes[strBytes.length -1] | 208); // Set bits 1101 0000

            } else {
                strBytes[strBytes.length -1] = (byte)(strBytes[strBytes.length -1] | 192); // Set bits 1100 0000
            }

        } else {
            throw new TypeConverterException("Unknown signing type");
        }

        //String test = debugBitmap(strBytes, strBytes.length -1, 1);
        return strBytes;
    }

    private String normalizeNumericSigning(String str, boolean signingPostfix) throws TypeConverterException {
        if (signingPostfix) {
            if (str.endsWith("-")) {
                str = '-' + str.substring(0, str.length() - 1);

            } else if (str.endsWith("+")) {
                str = str.substring(0, str.length() - 1);

            } else {
                throw new TypeConverterException("Missing sign char for value '" + str + "'");
            }

        } else {
            if (str.startsWith("+")) {
                str = str.substring(1, str.length());

            } else if (str.startsWith("-")) {
                // DO nothing

            } else {
                throw new TypeConverterException("Missing sign char for value '" + str + "'");
            }
        }
        return str;
    }

    private String debugBitmap(byte[] bytes, int index, int length) {
        String result = "";
        for(int i = index; i < length; i++) {
            result += ("0000000" + Integer.toBinaryString(bytes[i] & 0xFF)).replaceAll(".*(.{8})$", "$1");
        }
        return result;
    }

}
