package dk.nversion.copybook.converters;

import dk.nversion.copybook.exceptions.CopyBookException;
import sun.reflect.generics.reflectiveObjects.NotImplementedException;

import java.util.HashMap;
import java.util.Map;

public class IntegerToEnum extends IntegerToInteger {
    Object[] enumConstants;
    Map<Integer,Object> toEnumMap = new HashMap<>();
    Map<Object,byte[]> fromEnumMap = new HashMap<>();

    @Override
    public void initialize(TypeConverterConfig config) throws CopyBookException {
        super.initialize(config);
        enumConstants = type.getEnumConstants();
        if(TypeConverterEnum.class.isAssignableFrom(type)) {
            for (Object enumConstant : enumConstants) {
                int value = ((TypeConverterEnum) enumConstant).getValue();
                toEnumMap.put(value, enumConstant);
                fromEnumMap.put(enumConstant, Integer.toString(value).getBytes(this.charset));
            }
        }
    }

    @Override
    public void validate(Class type, int size, int decimals) throws TypeConverterException {
        if(!(Enum.class.isAssignableFrom(type))) {
            throw new TypeConverterException("Only supports converting to and from Enum or Integer");
        }
    }

    @Override
    public Object to(byte[] bytes, int offset, int length, int decimals, boolean removePadding) throws TypeConverterException {
        int i = (int)super.to(bytes, offset, length, decimals, removePadding);

        if(toEnumMap.size() > 0) {
            if (toEnumMap.containsKey(i)) {
                return toEnumMap.get(i);

            } else {
                throw new TypeConverterException("Unknown value for enum: " + i);
            }

        } else if(i < enumConstants.length) {
            return enumConstants[i];

        } else {
            throw new TypeConverterException("Unknown ordinal value for enum: " + i);
        }
    }

    @Override
    public byte[] from(Object value, int length, int decimals, boolean addPadding) throws TypeConverterException {
        if(value == null && this.defaultValue == null) {
            return null;
        }

        byte[] strBytes = fromEnumMap.get(value);
        if (strBytes.length > length) {
            throw new TypeConverterException("Field to small for value: " + length + " < " + strBytes.length);
        }

        if (addPadding) {
            strBytes = padBytes(strBytes, length);
        }

        return strBytes;
    }


}