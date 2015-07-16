package dk.nversion.copybook.record.test;

import dk.nversion.copybook.*;

@CopyBook()
@CopyBookPadding(fieldType = CopyBookFieldType.INT, right = true, character  = '0')
@CopyBookPadding(fieldType = CopyBookFieldType.STRING, right = true, character  = ' ')
public class RequestMessage {
    @CopyBookLine("02 TITLE PIC X(5).")
    public String title;
    @CopyBookPadding(fieldType = CopyBookFieldType.STRING, right = false, character  = '_')
    @CopyBookLine("02 BODY PIC X(10).")
    public String body;

    public RequestMessage() {
    }

    public RequestMessage(String title, String body) {
        this.title = title;
        this.body = body;
    }
}
