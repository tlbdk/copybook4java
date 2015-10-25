package dk.nversion.copybook.converters;

import dk.nversion.copybook.exceptions.CopyBookException;
import dk.nversion.copybook.exceptions.TypeConverterException;

// TODO: Implement
public interface TypeConverter {
    public abstract void initialize(TypeConverterConfig config) throws CopyBookException;
    public abstract void validate(Class type, int size, int decimals) throws TypeConverterException;
    public abstract Object to(byte[] bytes, int offset, int length, int decimals, boolean removePadding) throws TypeConverterException;
    public abstract byte[] from(Object value, int length, int decimals, boolean addPadding) throws TypeConverterException;
}
