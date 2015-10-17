package dk.nversion.copybook.converters;

import dk.nversion.copybook.CopyBookFieldSigningType;
import dk.nversion.copybook.exceptions.TypeConverterException;

import java.nio.charset.Charset;
import java.util.Arrays;

public abstract class TypeConverterBase {
    protected Charset charset;
    protected CopyBookFieldSigningType signingType;
    protected boolean rightPadding;
    protected char paddingChar;
    protected char nullFillerChar;

    public void setConfig(TypeConverterConfig config) {
        this.charset = config.getCharset();
        this.signingType = config.getSigningType();
        this.rightPadding = config.isRightPadding();
        this.paddingChar = config.getPaddingChar();
        this.nullFillerChar = config.getNullFillerChar();
    }

    public abstract void validate(Class type, int size, int decimals) throws TypeConverterException;
    public abstract Object to(byte[] bytes, int offset, int length, int decimals, boolean removePadding) throws TypeConverterException;
    public abstract byte[] from(Object value, int length, int decimals, boolean addPadding) throws TypeConverterException;

    public Object to(byte[] bytes, int decimals, boolean removePadding) throws TypeConverterException {
        return this.to(bytes, 0, bytes.length, decimals, removePadding);
    }

    protected byte[] padBytes(byte[] bytes, int length) {
        byte[] paddedStrBytes = new byte[length];
        Arrays.fill(paddedStrBytes, (byte)this.paddingChar);
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
                    if (bytes[offset + length -1] != (byte)this.paddingChar) {
                        break;
                    }
                }

            } else {
                int orgOffset = offset;
                for (; offset < length - minLength; offset++) {
                    if (bytes[offset] != (byte)this.paddingChar) {
                        break;
                    }
                }
                length -= offset - orgOffset;
            }
        }
        return new String(bytes, offset, length, charset);
    }
}
