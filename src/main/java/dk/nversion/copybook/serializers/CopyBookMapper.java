package dk.nversion.copybook.serializers;

import dk.nversion.copybook.converters.TypeConverterConfig;
import dk.nversion.copybook.exceptions.CopyBookException;

public interface CopyBookMapper {
    void initialize(CopyBookSerializerConfig config);
    <T> byte[] serialize(T obj);
    <T> T deserialize(byte[] bytes, Class<T> type);
    int getMaxRecordSize();
    void setMaxRecordSize(int size);
    int getMinRecordSize();
    void setMinRecordSize(int size);
}
