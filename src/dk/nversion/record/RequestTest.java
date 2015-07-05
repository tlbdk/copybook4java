package dk.nversion.record;

import dk.nversion.CopyBookField;

public class RequestTest {

    // Fields
    @CopyBookField("01 ID PIC 9(8).")
    public int id;
    @CopyBookField("01 CMD PIC X(10).")
    public String command;
    @CopyBookField("01 HELLO.")
    public RequestMessage hello;
    @CopyBookField("01 ARGCNT PIC 9(2).")
    public int args_count;
    @CopyBookField("01 MSGCNT PIC 9(2).")
    public int messages_count;
    @CopyBookField("01 ARGS OCCURS 10 TIMES.")
    @CopyBookField("02 ARG PIC X(8).")
    public String[] args;
    @CopyBookField("01 MSGS OCCURS 2 TIMES.")
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
