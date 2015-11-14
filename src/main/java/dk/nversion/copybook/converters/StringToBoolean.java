package dk.nversion.copybook.converters;

import dk.nversion.copybook.exceptions.TypeConverterException;
import sun.reflect.generics.reflectiveObjects.NotImplementedException;

public class StringToBoolean extends StringToString {
    private String[] values;

    @Override
    public void initialize(TypeConverterConfig config) {
        super.initialize(config);
        values = format.split("\\|");
        if(values.length != 2) {
            throw new TypeConverterException("A minimum of two values should be provided in the format with | as seperator");
        }
    }

    @Override
    public void validate(Class<?> type, int size, int decimals) {
        if(!(Boolean.class.equals(type) || Boolean.TYPE.equals(type))) {
            throw new TypeConverterException("Only supports converting to and from int or Integer");
        }
    }

    @Override
    public Object to(byte[] bytes, int offset, int length, int decimals, boolean removePadding) {
        String value = (String)super.to(bytes, offset, length, decimals, removePadding);
        if(values[0].equals(value)) {
            return true;

        } else if(values[1].equals(value)) {
            return false;

        } else {
            throw new TypeConverterException("Unknown true or false value: " + value);
        }
    }

    @Override
    public byte[] from(Object value, int length, int decimals, boolean addPadding) {
        return super.from((boolean)value ? values[0] : values[1], length, decimals, addPadding);
    }
}
