package dk.nversion.copybook.converters;

import dk.nversion.copybook.exceptions.TypeConverterException;

import java.util.HashMap;
import java.util.Map;

public class IntegerToTypeConverterIntEnum extends IntegerToInteger {
    Class<?> type;
    Object[] enumConstants;
    Map<Integer,Object> toEnumMap = new HashMap<>();
    Map<Object,byte[]> fromEnumMap = new HashMap<>();

    @Override
    public void validate(Class<?> type, int size, int decimals) {
        if(!(TypeConverterIntEnum.class.isAssignableFrom(type))) {
            throw new TypeConverterException("Only supports converting to and from TypeConverterIntEnum");
        }
        if(this.type != null && this.type != type) {
            throw new TypeConverterException("Type converter can only be used on " + type.getSimpleName() + " fields");
        }
        this.type = type;

        enumConstants = type.getEnumConstants();
        if(enumConstants == null || enumConstants.length == 0) {
            throw new TypeConverterException("Could not find any enum constants on type");
        }

        for (Object enumConstant : enumConstants) {
            int value = ((TypeConverterIntEnum) enumConstant).getValue();
            toEnumMap.put(value, enumConstant);
            fromEnumMap.put(enumConstant, Integer.toString(value).getBytes(this.charset));
        }
    }

    @Override
    public Object to(byte[] bytes, int offset, int length, int decimals, boolean removePadding) {
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
    public byte[] from(Object value, int length, int decimals, boolean addPadding) {
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
