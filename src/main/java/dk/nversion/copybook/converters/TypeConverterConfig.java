package dk.nversion.copybook.converters;

import dk.nversion.copybook.serializers.CopyBookFieldSigningType;

import java.nio.charset.Charset;

public class TypeConverterConfig {
    private Charset charset;
    private CopyBookFieldSigningType signingType;
    private boolean rightPadding;
    private char paddingChar;
    private char nullFillerChar;
    private String defaultValue;
    private String format;
    private Class type;

    public TypeConverterConfig() {
    }

    public Charset getCharset() {
        return charset;
    }

    public void setCharset(Charset charset) {
        this.charset = charset;
    }

    public CopyBookFieldSigningType getSigningType() {
        return signingType;
    }

    public void setSigningType(CopyBookFieldSigningType signingType) {
        this.signingType = signingType;
    }

    public boolean isRightPadding() {
        return rightPadding;
    }

    public void setRightPadding(boolean rightPadding) {
        this.rightPadding = rightPadding;
    }

    public char getPaddingChar() {
        return paddingChar;
    }

    public void setPaddingChar(char paddingChar) {
        this.paddingChar = paddingChar;
    }

    public char getNullFillerChar() {
        return nullFillerChar;
    }

    public void setNullFillerChar(char nullFillerChar) {
        this.nullFillerChar = nullFillerChar;
    }

    public String getDefaultValue() {
        return defaultValue;
    }

    public void setDefaultValue(String defaultValue) {
        this.defaultValue = defaultValue;
    }

    public String getFormat() {
        return format;
    }

    public void setFormat(String format) {
        this.format = format;
    }

    public Class getType() {
        return type;
    }

    public void setType(Class type) {
        this.type = type;
    }
}
