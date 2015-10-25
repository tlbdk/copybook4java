package dk.nversion.copybook.serializers;

import dk.nversion.copybook.exceptions.CopyBookException;

// TODO: Implement
public interface CopyBookSerializer {
    public abstract <T> byte[] serialize(T obj) throws CopyBookException;
    public abstract <T> T deserialize(byte[] bytes, Class<T> type) throws CopyBookException, InstantiationException;
}
