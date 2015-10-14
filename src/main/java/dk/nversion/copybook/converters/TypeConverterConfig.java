package dk.nversion.copybook.converters;

import dk.nversion.copybook.CopyBookFieldSigningType;

import java.nio.charset.Charset;

public class TypeConverterConfig {
    private Charset charset;
    private CopyBookFieldSigningType signingType;
    private boolean rightPadding;
    private char paddingChar;
    private char nullFillerChar;
    private int size;
    private int decimals;

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

    public int getSize() {
        return size;
    }

    public void setSize(int size) {
        this.size = size;
    }

    public int getDecimals() {
        return decimals;
    }

    public void setDecimals(int decimals) {
        this.decimals = decimals;
    }
}
