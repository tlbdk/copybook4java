package dk.nversion.copybook.serializers;

import dk.nversion.copybook.annotations.CopyBook;
import dk.nversion.copybook.annotations.CopyBookLine;
import dk.nversion.copybook.exceptions.CopyBookException;
import dk.nversion.old.packed.RequestTest;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import static org.junit.Assert.assertEquals;

public class CopyBookSerializerPackedFirstLevelTest {
    @Rule
    public ExpectedException expectedEx = ExpectedException.none();

    @CopyBook(charset = "UTF-8", type = PackedFirstLevelSerializer.class)
    public static class RequestTest {
        @CopyBookLine("01 ID PIC 9(8).")
        public int id;
        @CopyBookLine("01 CMD PIC X(10).")
        public String command;
        @CopyBookLine("01 HELLO.")
        public RequestMessage hello;
        @CopyBookLine("01 HELLOCNT PIC 9(2).")
        public int hellos_count;
        @CopyBookLine("01 ARGCNT PIC 9(2).")
        public int args_count;
        @CopyBookLine("01 MSGCNT PIC 9(2).")
        public int messages_count;
        @CopyBookLine("01 HELLOS OCCURS 3 TIMES.")
        public RequestMessage[] hellos;
        @CopyBookLine("01 ARGS OCCURS 10 TIMES.")
        @CopyBookLine("02 ARG PIC X(8).")
        public String[] args;
        @CopyBookLine("01 MSGS OCCURS 2 TIMES.")
        public RequestMessage[] messages;
    }

    @CopyBook()
    public static class RequestMessage {
        @CopyBookLine("02 TITLE PIC X(5).")
        public String title;
        @CopyBookLine("02 BODY PIC X(10).")
        public String body;

        public RequestMessage() {

        }

        public RequestMessage(String title, String body) {
            this.title = title;
            this.body = body;
        }
    }

    @Test
    public void testSerializeDeserialize() throws Exception {
        // Build test object
        RequestTest requestTest = new RequestTest();
        requestTest.id = 1;
        requestTest.command = "cmd1234";
        requestTest.hello = new RequestMessage("Helo", "Body1234");
        requestTest.hellos = new RequestMessage[] { new RequestMessage("abc", "1234ydob") };
        requestTest.hellos_count = requestTest.hellos.length;
        requestTest.args = new String[]{ "do", "stuff" };
        requestTest.args_count = requestTest.args.length;
        requestTest.messages = new RequestMessage[] { new RequestMessage("msg1", "stuff123"), new RequestMessage("msg2", "stuff1234") };
        requestTest.messages_count = requestTest.messages.length;

        // Serializer and Deserializer object to and from bytes
        CopyBookSerializer requestTestSerializer = new CopyBookSerializer(RequestTest.class, false);
        byte[] bytes = requestTestSerializer.serialize(requestTest);
        RequestTest requestTest1 = requestTestSerializer.deserialize(bytes, RequestTest.class);

        assertEquals(requestTest.id, requestTest1.id);
        assertEquals(requestTest.command, requestTest1.command);
        assertEquals(requestTest.hello.title, requestTest1.hello.title);
        assertEquals(requestTest.hello.body, requestTest1.hello.body);
        assertEquals(requestTest.messages_count, requestTest1.messages_count);
        assertEquals(requestTest.messages.length, requestTest1.messages.length);
        assertEquals(requestTest.messages[0].title, requestTest1.messages[0].title);
        assertEquals(requestTest.messages[0].body, requestTest1.messages[0].body);
        assertEquals(requestTest.messages[1].title, requestTest1.messages[1].title);
        assertEquals(requestTest.messages[1].body, requestTest1.messages[1].body);
        assertEquals(requestTest.args_count, requestTest1.args_count);
        assertEquals(requestTest.args.length, requestTest1.args.length);
        assertEquals(requestTest.args[0], requestTest1.args[0]);
        assertEquals(requestTest.args[1], requestTest1.args[1]);
    }

    @Test
    public void testSerializeDeserializeNullValues() throws Exception {
        // Build test object
        RequestTest requestTest = new RequestTest();
        requestTest.hellos = new RequestMessage[] { null };
        requestTest.hellos_count = requestTest.hellos.length;

        // Serializer and Deserializer object to and from bytes
        CopyBookSerializer requestTestSerializer = new CopyBookSerializer(RequestTest.class);
        byte[] bytes = requestTestSerializer.serialize(requestTest);
        RequestTest requestTest1 = requestTestSerializer.deserialize(bytes, RequestTest.class);

        assertEquals(0, requestTest1.args.length);
    }

    @Test
    public void testSerializeDeserializeEmptyList() throws Exception {
        // Build test object
        RequestTest requestTest = new RequestTest();
        requestTest.messages = new RequestMessage[] {};
        requestTest.args = new String[]{};

        // Serializer and Deserializer object to and from bytes
        CopyBookSerializer requestTestSerializer = new CopyBookSerializer(RequestTest.class, true);
        byte[] bytes = requestTestSerializer.serialize(requestTest);
        RequestTest requestTest1 = requestTestSerializer.deserialize(bytes, RequestTest.class);

        assertEquals(0, requestTest1.args.length);
        assertEquals(0, requestTest1.messages.length);
    }

    @org.junit.Test
    public void testSerializeDeserializeSeparatorByteException() throws Exception {
        expectedEx.expect(CopyBookException.class);
        expectedEx.expectMessage("contains the separator char");
        CopyBookSerializer requestTestSerializer = new CopyBookSerializer(RequestTest.class);
        RequestTest test1 = new RequestTest();
        test1.command = "c" + '\u000b' + "extra";
        byte[] test1data = requestTestSerializer.serialize(test1);
    }


}