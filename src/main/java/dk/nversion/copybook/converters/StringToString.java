package dk.nversion.copybook.converters;

public class StringToString extends TypeConverterBase {

    public StringToString(TypeConverterConfig config) {
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
