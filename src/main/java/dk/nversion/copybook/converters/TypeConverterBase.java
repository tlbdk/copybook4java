package dk.nversion.copybook.converters;

import dk.nversion.copybook.CopyBookFieldSigningType;

import java.nio.charset.Charset;

public abstract class TypeConverterBase {
    protected Charset charset;
    protected CopyBookFieldSigningType signingType;
    protected boolean rightPadding;
    protected char paddingChar;
    protected char nullFillerChar;

    public TypeConverterBase(TypeConverterConfig config) {
        this.charset = config.getCharset();
        this.signingType = config.getSigningType();
        this.rightPadding = config.isRightPadding();
        this.paddingChar = config.getPaddingChar();
        this.nullFillerChar = config.getNullFillerChar();
    }

    public abstract Object to(byte[] bytes, int offset, int length, boolean removePadding);
    public abstract byte[] from(Object value, boolean addPadding);
}
