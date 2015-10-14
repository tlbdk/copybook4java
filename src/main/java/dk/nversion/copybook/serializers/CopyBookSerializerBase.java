package dk.nversion.copybook.serializers;


import dk.nversion.copybook.exceptions.CopyBookException;

import java.util.List;

public abstract class CopyBookSerializerBase {
    protected CopyBookSerializerConfig config;
    protected List<CopyBookField> fields;

    public CopyBookSerializerBase(CopyBookSerializerConfig config, List<CopyBookField> fields) {
        this.config = config;
        this.fields = fields;
    }

    public abstract <T> byte[] serialize(T obj) throws CopyBookException;
    public abstract <T> T deserialize(byte[] bytes, Class<T> type) throws CopyBookException, InstantiationException;
}
