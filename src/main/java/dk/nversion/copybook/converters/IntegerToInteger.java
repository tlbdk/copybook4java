package dk.nversion.copybook.converters;

public class IntegerToInteger extends TypeConverterBase {
    public IntegerToInteger(TypeConverterConfig config) {
        super(config);
    }

    public Object to(byte[] bytes, int offset, int length, boolean removePadding) {
        return Integer.parseInt(new String(bytes, offset, length, charset));
    }

    public byte[] from(Object value, boolean addPadding) {
       return String.valueOf((int)value).getBytes();
    }
}
