package dk.nversion.copybook.serializers;

import dk.nversion.copybook.CopyBookException;

import java.lang.reflect.InvocationTargetException;

public class CopyBookSerializer {
    private CopyBookSerializerBase serializer;

    public CopyBookSerializer(Class type) throws CopyBookException {
        CopyBookParser parser = new CopyBookParser(type);

        try {
            serializer = (CopyBookSerializerBase)parser.getSerializerClass().getDeclaredConstructor(CopyBookSerializerConfig.class).newInstance(parser.getConfig());

        } catch (InstantiationException | IllegalAccessException | InvocationTargetException | NoSuchMethodException e) {
            throw new CopyBookException("Failed to load Serialization class");
        }
    }

    public <T> byte[] serialize(T obj) throws CopyBookException {
        return serializer.serialize(obj);
    }

    public  <T> T deserialize(byte[] bytes, Class<T> type) throws CopyBookException, InstantiationException {
        return serializer.deserialize(bytes, type);
    }

}
