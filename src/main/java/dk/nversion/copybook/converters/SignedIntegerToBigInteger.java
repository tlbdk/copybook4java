package dk.nversion.copybook.converters;

import dk.nversion.copybook.CopyBookFieldSigningType;

import java.nio.charset.Charset;

public class SignedIntegerToBigInteger extends TypeConverter {
    public SignedIntegerToBigInteger(TypeConverterConfig config) {
        super(config);
    }

    @Override
    public Object to(byte[] bytes, int offset, int length) {
        return null;
    }

    @Override
    public byte[] from(Object value) {
        return new byte[0];
    }


}
