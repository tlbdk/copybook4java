package dk.nversion.copybook.serializers;


import dk.nversion.copybook.exceptions.CopyBookException;

import java.util.List;

public abstract class CopyBookSerializerBase {
    protected CopyBookSerializerConfig config;
    protected List<CopyBookField> fields;
    protected boolean debug;

    public CopyBookSerializerBase(CopyBookSerializerConfig config) {
        this.config = config;
        this.fields = config.getFields();
        this.debug = config.isDebug();
    }

    public abstract <T> byte[] serialize(T obj) throws CopyBookException;
    public abstract <T> T deserialize(byte[] bytes, Class<T> type) throws CopyBookException, InstantiationException;
}
