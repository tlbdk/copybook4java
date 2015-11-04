package dk.nversion.copybook;

import dk.nversion.ByteUtils;
import dk.nversion.copybook.exceptions.CopyBookException;
import dk.nversion.copybook.serializers.CopyBookMapper;
import dk.nversion.copybook.serializers.CopyBookParser;

import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.InvocationTargetException;
import java.util.List;

public class CopyBookSerializer {
    private CopyBookMapper serializer;

    public CopyBookSerializer(Class<?> type) throws CopyBookException {
        this(type, false);
    }

    public CopyBookSerializer(Class<?> type, boolean debug) throws CopyBookException {
        this(type, null, debug);
    }

    public CopyBookSerializer(Class<?> type, Class<CopyBookMapper> mapper, boolean debug) throws CopyBookException {
        CopyBookParser parser = new CopyBookParser(type, debug);
        try {
            if(mapper != null) {
                serializer = mapper.getDeclaredConstructor().newInstance();

            } else {
                serializer = parser.getSerializerClass().getDeclaredConstructor().newInstance();
            }
            serializer.initialize(parser.getConfig());

        } catch (InstantiationException | IllegalAccessException | InvocationTargetException | NoSuchMethodException e) {
            throw new CopyBookException("Failed to load Serialization class("+ parser.getSerializerClass().getSimpleName() +")", e);
        }
    }

    public <T> byte[] serialize(T obj) throws CopyBookException {
        return serializer.serialize(obj);
    }

    public <T> T deserialize(byte[] bytes, Class<T> type) throws CopyBookException, InstantiationException {
        return serializer.deserialize(bytes, type);
    }

    public <T> T deserialize(InputStream inputStream, Class<T> type) throws CopyBookException, InstantiationException, IOException {
        return deserialize(ByteUtils.toByteArray(inputStream), type);
    }

    public List<CopyBookException> getErrors() {
        return null; // TODO: Implement depending on errorValue
    }

    public int getMinRecordSize() {
        return serializer.getMinRecordSize();
    }

    public int getMaxRecordSize() {
        return serializer.getMaxRecordSize();
    }

}
