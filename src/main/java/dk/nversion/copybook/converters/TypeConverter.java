package dk.nversion.copybook.converters;

import dk.nversion.copybook.exceptions.TypeConverterException;

public interface TypeConverter {
    void initialize(TypeConverterConfig config);
    void validate(Class<?> type, int size, int decimals);
    Object to(byte[] bytes, int offset, int length, int decimals, boolean removePadding);
    byte[] from(Object value, int length, int decimals, boolean addPadding);
}