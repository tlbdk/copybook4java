package dk.nversion.copybook.serializers;

import dk.nversion.copybook.converters.TypeConverterConfig;
import dk.nversion.copybook.exceptions.CopyBookException;

// TODO: Implement
public interface CopyBookMapper {
    void initialize(CopyBookSerializerConfig config) throws CopyBookException;
    <T> byte[] serialize(T obj) throws CopyBookException;
    <T> T deserialize(byte[] bytes, Class<T> type) throws CopyBookException, InstantiationException;
    int getMaxRecordSize();
    void setMaxRecordSize(int size);
    int getMinRecordSize();
    void setMinRecordSize(int size);
}
