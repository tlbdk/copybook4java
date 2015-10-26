package dk.nversion.copybook.serializers;

import dk.nversion.copybook.exceptions.CopyBookException;

// TODO: Implement
public interface CopyBookSerializer {
    public abstract <T> byte[] serialize(T obj) throws CopyBookException;
    public abstract <T> T deserialize(byte[] bytes, Class<T> type) throws CopyBookException, InstantiationException;
    public abstract int getMaxRecordSize();
    public abstract void setMaxRecordSize(int size);
    public abstract int getMinRecordSize();
    public abstract void setMinRecordSize(int size);
}
