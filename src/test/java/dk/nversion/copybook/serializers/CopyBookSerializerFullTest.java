package dk.nversion.copybook.serializers;

import dk.nversion.copybook.annotations.CopyBook;
import dk.nversion.copybook.annotations.CopyBookLine;
import dk.nversion.copybook.exceptions.TypeConverterException;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import static org.junit.Assert.assertEquals;

public class CopyBookSerializerFullTest {
    @Rule
    public ExpectedException expectedEx = ExpectedException.none();

    @CopyBook(charset = "UTF-8", type = FullSerializer.class)
    public static class RequestTest {
        @CopyBookLine("01 ID PIC 9(8).")
        public int id;
        @CopyBookLine("01 CMD PIC X(10).")
        public String command;
        @CopyBookLine("01 HELLO.")
        public RequestMessage hello;
        @CopyBookLine("01 ARGCNT PIC 9(2).")
        public int args_count;
        @CopyBookLine("01 MSGCNT PIC 9(2).")
        public int messages_count;
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
        requestTest.command = "cmd1234()";
        requestTest.hello = new RequestMessage("Hello", "Body1234");
        requestTest.messages = new RequestMessage[] { new RequestMessage("msg1", "stuff123"), new RequestMessage("msg2", "stuff12345") };
        requestTest.messages_count = 2;
        requestTest.args = new String[]{ "do", "stuff" };
        requestTest.args_count = 2;

        // Serializer and Deserializer object to and from bytes
        CopyBookSerializer requestTestSerializer = new CopyBookSerializer(RequestTest.class, true);
        byte[] bytes = requestTestSerializer.serialize(requestTest);
        RequestTest requestTest1 = requestTestSerializer.deserialize(bytes, RequestTest.class);

        assertEquals(requestTest.id, requestTest1.id);
        assertEquals(requestTest.command, requestTest1.command);
        assertEquals(requestTest.hello.title, requestTest1.hello.title);
        assertEquals(requestTest.hello.body, requestTest1.hello.body);
        assertEquals(requestTest.messages_count, requestTest1.messages_count);
        assertEquals(requestTest.messages[0].title, requestTest1.messages[0].title);
        assertEquals(requestTest.messages[0].body, requestTest1.messages[0].body);
        assertEquals(requestTest.messages[1].title, requestTest1.messages[1].title);
        assertEquals(requestTest.messages[1].body, requestTest1.messages[1].body);
        assertEquals(requestTest.args_count, requestTest1.args_count);
        assertEquals(requestTest.args[0], requestTest.args[0]);
        assertEquals(requestTest.args[1], requestTest.args[1]);
    }
}