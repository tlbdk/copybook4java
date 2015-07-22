package dk.nversion.copybook;

import dk.nversion.copybook.full.RequestMessage;
import dk.nversion.copybook.full.RequestTest;

import static org.junit.Assert.assertEquals;

public class SerializerFullEmptyListTest {

    @org.junit.Test
    public void testSerializeDeserialize() throws Exception {
        CopyBookSerializer requestTestSerializer = new CopyBookSerializer(RequestTest.class);
        RequestTest test1 = new RequestTest().builder()
                .setId(1)
                .setCommand("cmd1234()")
                .setHello(new RequestMessage("Hello", "Body1234"))
                .setMessages(new RequestMessage[]{new RequestMessage("msg1", "stuff123"), new RequestMessage("msg2", "stuff12345")})
                .setArgs(new String[]{})
                .build();

        byte[] test1data = requestTestSerializer.serialize(test1);
        RequestTest test2 = requestTestSerializer.deserialize(test1data, RequestTest.class);

        assertEquals(test1, test2);
    }
}