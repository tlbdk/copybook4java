package dk.nversion.copybook.converters;

import dk.nversion.copybook.serializers.CopyBookFieldSigningType;
import dk.nversion.copybook.exceptions.CopyBookException;
import dk.nversion.copybook.exceptions.TypeConverterException;

import java.nio.charset.Charset;
import java.util.Arrays;

public abstract class TypeConverterBase {
    protected Charset charset;
    protected CopyBookFieldSigningType signingType;
    protected boolean rightPadding;
    protected byte paddingByte;
    protected byte nullFillerByte;

    public void setConfig(TypeConverterConfig config) throws CopyBookException {
        this.charset = config.getCharset();
        this.signingType = config.getSigningType();
        this.rightPadding = config.isRightPadding();

        byte[] paddingBytes = ( config.getPaddingChar() + "").getBytes(this.charset);
        if(paddingBytes.length > 1) {
            throw new CopyBookException("Selected charset and padding char is more than 1 byte long");
        }
        this.paddingByte = paddingBytes[0];

        byte[] nullFillerBytes = (config.getNullFillerChar() + "").getBytes(charset);
        if(nullFillerBytes.length > 1) {
            throw new CopyBookException("Selected charset and null filler char is more than 1 byte long");
        }
        this.nullFillerByte = nullFillerBytes[0];
    }

    public abstract void validate(Class type, int size, int decimals) throws TypeConverterException;
    public abstract Object to(byte[] bytes, int offset, int length, int decimals, boolean removePadding) throws TypeConverterException;
    public abstract byte[] from(Object value, int length, int decimals, boolean addPadding) throws TypeConverterException;

    public Object to(byte[] bytes, int decimals, boolean removePadding) throws TypeConverterException {
        return this.to(bytes, 0, bytes.length, decimals, removePadding);
    }

    protected byte[] padBytes(byte[] bytes, int length) {
        byte[] paddedStrBytes = new byte[length];
        Arrays.fill(paddedStrBytes, this.paddingByte);
        if (this.rightPadding) {
            System.arraycopy(bytes, 0, paddedStrBytes, 0, bytes.length);
        } else {
            System.arraycopy(bytes, 0, paddedStrBytes, paddedStrBytes.length - bytes.length, bytes.length);
        }
        return paddedStrBytes;
    }

    protected String getString(byte[] bytes, int offset, int length, boolean removePadding, int minLength) throws TypeConverterException {
        if(removePadding) {
            if(this.rightPadding) {
                for (;length > minLength; length--) {
                    if (bytes[offset + length -1] != this.paddingByte) {
                        break;
                    }
                }

            } else {
                int orgOffset = offset;
                for (; offset < length - minLength; offset++) {
                    if (bytes[offset] != this.paddingByte) {
                        break;
                    }
                }
                length -= offset - orgOffset;
            }
        }
        return new String(bytes, offset, length, charset);
    }
}
