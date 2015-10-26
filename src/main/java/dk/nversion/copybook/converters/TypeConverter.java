package dk.nversion.copybook.converters;

import dk.nversion.copybook.exceptions.CopyBookException;

public interface TypeConverter {
    void initialize(TypeConverterConfig config) throws CopyBookException;
    void validate(Class type, int size, int decimals) throws TypeConverterException;
    Object to(byte[] bytes, int offset, int length, int decimals, boolean removePadding) throws TypeConverterException;
    byte[] from(Object value, int length, int decimals, boolean addPadding) throws TypeConverterException;
}