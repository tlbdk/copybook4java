package dk.nversion.copybook.serializers;

import dk.nversion.copybook.*;
import java.io.InputStream;
import java.util.List;

public class FullSerializer extends CopyBookSerializerBase { // TODO: Add extends and implements

    public FullSerializer(Class type) throws CopyBookException {
        super(type);
    }

    public <T> byte[] serialize(T obj) throws CopyBookException {
        return null;
    }

    public <T> T deserialize(byte[] bytes, Class<T> type) throws CopyBookException, InstantiationException {
        return null;
    }

}
