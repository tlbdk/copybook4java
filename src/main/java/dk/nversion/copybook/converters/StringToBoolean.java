package dk.nversion.copybook.converters;

import sun.reflect.generics.reflectiveObjects.NotImplementedException;

public class StringToBoolean extends TypeConverterBase {

    @Override
    public void validate(Class<?> type, int size, int decimals) throws TypeConverterException {
        throw new NotImplementedException();
    }

    @Override
    public Object to(byte[] bytes, int offset, int length, int decimals, boolean removePadding) throws TypeConverterException {
        return null;
    }

    @Override
    public byte[] from(Object value, int length, int decimals, boolean addPadding) throws TypeConverterException {
        return new byte[0];
    }


}
