/*
 * Copyright (c) 2015. Troels Liebe Bentsen <tlb@nversion.dk>
 * Licensed under the MIT license (LICENSE.txt)
 */

package dk.nversion.copybook.converters;

import dk.nversion.ByteUtils;
import dk.nversion.copybook.exceptions.TypeConverterException;

public class IntegerToInteger extends TypeConverterBase {
    @Override
    public void validate(Class<?> type, int size, int decimal) {
        if(size > 9) {
            throw new TypeConverterException("int is not large enough to hold possible value");
        }
        if(!(Integer.class.equals(type) || Integer.TYPE.equals(type))) {
            throw new TypeConverterException("Only supports converting to and from int or Integer");
        }
    }

    @Override
    public Object to(byte[] bytes, int offset, int length, int decimals, boolean removePadding) {
        if(this.defaultValue != null && ByteUtils.allEquals(bytes, this.nullFillerByte, offset, bytes.length)) { // All of value is null filler
            return Integer.parseInt(this.defaultValue);

        } else {
            return Integer.parseInt(getIntegerString(bytes, offset, length, removePadding));
        }
    }

    @Override
    public byte[] from(Object value, int length, int decimals, boolean addPadding) {
        if(value == null && this.defaultValue == null) {
            return null;
        }

        int i = value != null ? (int) value : Integer.parseInt(this.defaultValue);
        if (i < 0) {
            throw new TypeConverterException("Number can not be negative");
        }

        byte[] strBytes = Integer.toString(i).getBytes(this.charset);
        if (strBytes.length > length) {
            throw new TypeConverterException("Field to small for value: " + length + " < " + strBytes.length);
        }
        if (addPadding) {
            strBytes = padBytes(strBytes, length);
        }

        return strBytes;

    }

    protected String getIntegerString(byte[] bytes, int offset, int length, boolean removePadding) {
        String strValue = getString(bytes, offset, length, removePadding, 1);
        if(strValue.startsWith("-") || strValue.endsWith("-")) {
            throw new TypeConverterException("Integer value can not start or end with -");
        }
        return strValue;
    }
}