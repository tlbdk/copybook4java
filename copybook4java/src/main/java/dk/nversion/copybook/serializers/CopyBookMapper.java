/*
 * Copyright (c) 2015. Troels Liebe Bentsen <tlb@nversion.dk>
 * Licensed under the MIT license (LICENSE.txt)
 */

package dk.nversion.copybook.serializers;

public interface CopyBookMapper {
    void initialize(CopyBookSerializerConfig config);
    <T> byte[] serialize(T obj);
    <T> T deserialize(byte[] bytes, Class<T> type);
    int getMaxRecordSize();
    void setMaxRecordSize(int size);
    int getMinRecordSize();
    void setMinRecordSize(int size);
}
