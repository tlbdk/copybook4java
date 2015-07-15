package dk.nversion.record;

import dk.nversion.CopyBookLine;

public class RequestTest {

    // Fields
    @CopyBookLine("01 ID PIC 9(8). * id(leftpadding:32)")
    private int id;
    @CopyBookLine("01 CMD PIC X(10). * command(leftpadding:32)")
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
