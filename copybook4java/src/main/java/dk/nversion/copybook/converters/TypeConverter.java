/*
 * Copyright (c) 2015. Troels Liebe Bentsen <tlb@nversion.dk>
 * Licensed under the MIT license (LICENSE.txt)
 */

package dk.nversion.copybook.converters;

public interface TypeConverter {
    void initialize(TypeConverterConfig config);
    void validate(Class<?> type, int size, int decimals);
    TypeConverter copy(Class<?> type) throws IllegalAccessException, InstantiationException;
    Object to(byte[] bytes, int offset, int length, int decimals, boolean removePadding);
    byte[] from(Object value, int length, int decimals, boolean addPadding);
}