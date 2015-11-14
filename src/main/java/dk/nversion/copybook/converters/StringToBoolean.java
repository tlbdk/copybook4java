package dk.nversion.copybook.converters;

import dk.nversion.copybook.exceptions.TypeConverterException;
import sun.reflect.generics.reflectiveObjects.NotImplementedException;

public class StringToBoolean extends TypeConverterBase {

    @Override
    public void validate(Class<?> type, int size, int decimals) {
        throw new NotImplementedException();
    }

    @Override
    public Object to(byte[] bytes, int offset, int length, int decimals, boolean removePadding) {
        return null;
    }

    @Override
    public byte[] from(Object value, int length, int decimals, boolean addPadding) {
        return new byte[0];
    }


}
