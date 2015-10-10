package dk.nversion.copybook.converters;

import java.nio.charset.Charset;

public class IntegerToInteger extends TypeConverter {
    IntegerToInteger(TypeConverterConfig config) {
        super(config);
    }

    public Object to(byte[] bytes, int offset, int length) {
        return Integer.parseInt(new String(bytes, offset, length, charset));
    }

    public byte[] from(Object value) {
       return String.valueOf((int)value).getBytes();
    }
}
