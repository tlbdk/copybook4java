package dk.nversion.old;

import dk.nversion.copybook.CopyBookSerializer;
import dk.nversion.copybook.exceptions.CopyBookException;
import dk.nversion.copybook.packed.RequestMessage;
import dk.nversion.copybook.packed.RequestTest;
import org.junit.Rule;
import org.junit.rules.ExpectedException;

import static org.junit.Assert.assertEquals;

public class SerializerPackedTest {

    @Rule
    public ExpectedException expectedEx = ExpectedException.none();

    @org.junit.Test
    public void testSerializeDeserialize() throws Exception {
        CopyBookSerializer requestTestSerializer = new CopyBookSerializer(RequestTest.class);
        RequestTest test1 = new RequestTest().builder()
                .setId(1)
                .setCommand("cmd1234()")
                .setHello(new RequestMessage("Hello", "Body1234"))
                .setMessages(new RequestMessage[]{new RequestMessage("msg1", "stuff123"), new RequestMessage("msg2", "stuff12345")})
                .setArgs(new String[]{"do", "stuff"})
                .build();

        byte[] test1data = requestTestSerializer.serialize(test1);
        RequestTest test2 = requestTestSerializer.deserialize(test1data, RequestTest.class);

        assertEquals(test1, test2);
    }

    @org.junit.Test
    public void testSerializeDeserializeSeparatorByteException() throws Exception {
        expectedEx.expect(CopyBookException.class);
        expectedEx.expectMessage("contains the separator char");
        CopyBookSerializer requestTestSerializer = new CopyBookSerializer(RequestTest.class);
        RequestTest test1 = new RequestTest().builder()
                .setCommand("c" + '\u000b' + "extra")
                .build();
        byte[] test1data = requestTestSerializer.serialize(test1);
    }

}