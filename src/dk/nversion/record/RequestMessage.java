package dk.nversion.record;

import dk.nversion.CopyBookField;

public class RequestMessage {
    @CopyBookField("02 TITLE PIC X(20)")
    public String title;
    @CopyBookField("02 BODY PIC X(200)")
    public String body;

    public RequestMessage(String title, String body) {
        this.title = title;
        this.body = body;
    }
}
