package dk.nversion.record;

import dk.nversion.CopyBookLine;

public class RequestTest {

    // Fields
    @CopyBookLine("01 ID PIC 9(8). * n:id")
    public int id;
    @CopyBookLine("01 CMD PIC X(10).* n:command")
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

    // Constructors
    public RequestTest() {

    }

    public RequestTest(int id, String command, String[] args) {
        this.id = id;
        this.command = command;
        this.args = args;
    }

    // Setters/Getters
    public int getId() {
        return this.id;
    }

    public void setId(int id) {
        this.id = id;
    }

    public String getCommand() {
        return this.command;
    }

    public void setCommand(String command) {
        this.command = command;
    }

    public String[] getArgs() {
        return this.args;
    }

    public void setArgs(String[] args) {
        this.args = args;
    }
}
