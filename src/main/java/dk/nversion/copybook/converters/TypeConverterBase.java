package dk.nversion.copybook.converters;

import dk.nversion.copybook.exceptions.TypeConverterException;
import dk.nversion.copybook.serializers.CopyBookFieldSigningType;

import java.nio.charset.Charset;
import java.util.Arrays;

public abstract class TypeConverterBase implements TypeConverter {
    protected Charset charset;
    protected CopyBookFieldSigningType signingType;
    protected boolean rightPadding;
    protected byte paddingByte;
    protected byte nullFillerByte;
    protected String defaultValue;
    protected String format;
    protected TypeConverterConfig config;

    @Override
    public void initialize(TypeConverterConfig config) {
        this.config = config;
        this.charset = config.getCharset();
        this.signingType = config.getSigningType();
        this.rightPadding = config.isRightPadding();

        byte[] paddingBytes = ( config.getPaddingChar() + "").getBytes(this.charset);
        if(paddingBytes.length > 1) {
            throw new TypeConverterException("Selected charset and padding char is more than 1 byte long");
        }
        this.paddingByte = paddingBytes[0];

        byte[] nullFillerBytes = (config.getNullFillerChar() + "").getBytes(charset);
        if(nullFillerBytes.length > 1) {
            throw new TypeConverterException("Selected charset and null filler char is more than 1 byte long");
        }
        this.nullFillerByte = nullFillerBytes[0];
        this.defaultValue = config.getDefaultValue();
        this.format = config.getFormat();
    }

    @Override
    public TypeConverter copy(Class<?> type) throws IllegalAccessException, InstantiationException {
        TypeConverter newConverter = this.getClass().newInstance();
        newConverter.initialize(config);
        return newConverter;
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

    protected String getString(byte[] bytes, int offset, int length, boolean removePadding, int minLength) {
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
