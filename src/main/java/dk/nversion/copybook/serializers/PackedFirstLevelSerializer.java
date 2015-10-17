package dk.nversion.copybook.serializers;

import dk.nversion.copybook.exceptions.CopyBookException;

import java.util.List;

public class PackedFirstLevelSerializer extends CopyBookSerializerBase {

    public PackedFirstLevelSerializer(CopyBookSerializerConfig config) {
        super(config);
    }

    @Override
    public <T> byte[] serialize(T obj) throws CopyBookException {
        return new byte[0];
    }

    @Override
    public <T> T deserialize(byte[] bytes, Class<T> type) throws CopyBookException, InstantiationException {
        return null;
    }
}
