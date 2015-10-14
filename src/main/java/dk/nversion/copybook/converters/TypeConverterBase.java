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
    public abstract Object to(byte[] bytes, int offset, int length, boolean removePadding) throws TypeConverterException;
    public abstract byte[] from(Object value, int length, boolean addPadding) throws TypeConverterException;

    public Object to(byte[] bytes, boolean removePadding) throws TypeConverterException {
        return this.to(bytes, 0, bytes.length, removePadding);
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

    protected int decrementLengthWithPaddingLength(byte[] bytes, int offset, int length, int minLength) {
        if(this.rightPadding) {
            for (int i = offset + length - 1; i > offset + minLength; i--) {
                if (bytes[i] != (byte)this.paddingChar) {
                    return i;
                }
            }
            return offset + minLength;

        } else {
            return length;
        }
    }

    protected int incrementOffsetWithPaddingLength(byte[] bytes, int offset, int length, int minLength) {
        if(this.rightPadding) {
            return offset;

        } else {
            for (int i = offset; i < length; i++) {
                if (bytes[i] != (byte)this.paddingChar) {
                    return  i;
                }
            }
            return minLength;
        }
    }
}
