package dk.nversion.copybook.converters;

public class SignedIntegerToLong extends TypeConverterBase {
    public SignedIntegerToLong(TypeConverterConfig config) {
        super(config);
    }

    @Override
    public Object to(byte[] bytes, int offset, int length, boolean removePadding) {
        return null;
    }

    @Override
    public byte[] from(Object value, boolean addPadding) {
        return new byte[0];
    }
}
