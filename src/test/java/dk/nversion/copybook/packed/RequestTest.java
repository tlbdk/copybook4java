package dk.nversion.copybook.packed;

import dk.nversion.copybook.*;
import dk.nversion.copybook.annotations.CopyBook;
import dk.nversion.copybook.annotations.CopyBookLine;
import dk.nversion.copybook.serializers.PackedFirstLevelSerializer;

import java.util.Arrays;

@CopyBook(type = PackedFirstLevelSerializer.class)
public class RequestTest {
    // Fields
    @CopyBookLine("01 ID PIC 9(8).")
    private int id;
    @CopyBookLine("01 CMD PIC X(10).")
    private String command;
    @CopyBookLine("01 HELLO.")
    private RequestMessage hello;
    @CopyBookLine("01 ARGCNT PIC 9(2).")
    private int args_count;
    @CopyBookLine("01 MSGCNT PIC 9(2).")
    private int messages_count;
    @CopyBookLine("01 ARGS OCCURS 10 TIMES.")
    @CopyBookLine("02 ARG PIC X(8).")
    private String[] args;
    @CopyBookLine("01 MSGS OCCURS 2 TIMES.")
    private RequestMessage[] messages;

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        RequestTest that = (RequestTest) o;

        if (id != that.id) return false;
        if (args_count != that.args_count) return false;
        if (messages_count != that.messages_count) return false;
        if (command != null ? !command.equals(that.command) : that.command != null) return false;
        if (hello != null ? !hello.equals(that.hello) : that.hello != null) return false;
        // Probably incorrect - comparing Object[] arrays with Arrays.equals
        if (!Arrays.equals(args, that.args)) return false;
        // Probably incorrect - comparing Object[] arrays with Arrays.equals
        return Arrays.equals(messages, that.messages);

    }

    @Override
    public int hashCode() {
        int result = id;
        result = 31 * result + (command != null ? command.hashCode() : 0);
        result = 31 * result + (hello != null ? hello.hashCode() : 0);
        result = 31 * result + args_count;
        result = 31 * result + messages_count;
        result = 31 * result + (args != null ? Arrays.hashCode(args) : 0);
        result = 31 * result + (messages != null ? Arrays.hashCode(messages) : 0);
        return result;
    }

    // Constructors
    public RequestTest() {

    }

    // Setters/Getters
    public int getId() {
        return this.id;
    }
    public String getCommand() {
        return this.command;
    }
    public RequestMessage getHello() {
        return this.hello;
    }
    public String[] getArgs() {
        return this.args;
    }
    public RequestMessage[] getMessages() {
        return this.messages;
    }

    // Builder
    public static RequestTestBuilder builder() {
        return new RequestTestBuilder();
    }

    public static final class RequestTestBuilder {

        private final RequestTest requestTest = new RequestTest();

        public RequestTestBuilder setId(int id) {
            requestTest.id = id;
            return this;
        }

        public RequestTestBuilder setCommand(String command) {
            requestTest.command = command;
            return this;
        }

        public RequestTestBuilder setHello(RequestMessage message) {
            requestTest.hello = message;
            return this;
        }

        public RequestTestBuilder setArgs(String[] args) {
            requestTest.args = args;
            requestTest.args_count = args.length;
            return this;
        }

        public RequestTestBuilder setMessages(RequestMessage[] messages) {
            requestTest.messages = messages;
            requestTest.messages_count = messages.length;
            return this;
        }

        public RequestTest build() {
            return requestTest;
        }
    }
}
