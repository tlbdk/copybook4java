package dk.nversion.record;

import dk.nversion.CopyBookLine;

public class RequestMessage {
    @CopyBookLine("02 TITLE PIC X(5)")
    public String title;
    @CopyBookLine("02 BODY PIC X(10)")
    public String body;

    public RequestMessage() {
    }

    public RequestMessage(String title, String body) {
        this.title = title;
        this.body = body;
    }
}
