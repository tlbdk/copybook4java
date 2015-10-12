package dk.nversion.copybook.serializers;

import dk.nversion.copybook.*;

import java.util.List;

public class FullSerializer extends CopyBookSerializerBase {
    public FullSerializer(CopyBookSerializerConfig config, List<CopyBookField> fields) {
        super(config, fields);
    }

    public <T> byte[] serialize(T obj) throws CopyBookException {
        return null;
    }

    public <T> T deserialize(byte[] bytes, Class<T> type) throws CopyBookException, InstantiationException {
        return null;
    }

}
