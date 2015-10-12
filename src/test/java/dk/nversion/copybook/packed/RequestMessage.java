package dk.nversion.copybook.packed;

import dk.nversion.copybook.*;
import dk.nversion.copybook.annotations.CopyBook;
import dk.nversion.copybook.annotations.CopyBookFieldFormat;
import dk.nversion.copybook.annotations.CopyBookLine;
import dk.nversion.copybook.converters.IntegerToInteger;
import dk.nversion.copybook.converters.StringToString;

@CopyBook()
@CopyBookFieldFormat(type = IntegerToInteger.class, rightPadding = true, paddingChar = '0', nullFillerChar = (byte)0, signingType = CopyBookFieldSigningType.PREFIX)
@CopyBookFieldFormat(type = StringToString.class, rightPadding = true, paddingChar = ' ', nullFillerChar = (byte)0, signingType = CopyBookFieldSigningType.PREFIX)
public class RequestMessage {
    @CopyBookLine("02 TITLE PIC X(10).")
    public String title;
    @CopyBookFieldFormat(type = StringToString.class, rightPadding = false, paddingChar = '_', nullFillerChar = (byte)0, signingType = CopyBookFieldSigningType.PREFIX)
    @CopyBookLine("02 BODY PIC X(20).")
    public String body;

    public RequestMessage() {
    }

    public RequestMessage(String title, String body) {
        this.title = title;
        this.body = body;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        RequestMessage that = (RequestMessage) o;

        if (title != null ? !title.equals(that.title) : that.title != null) return false;
        return !(body != null ? !body.equals(that.body) : that.body != null);

    }

    @Override
    public int hashCode() {
        int result = title != null ? title.hashCode() : 0;
        result = 31 * result + (body != null ? body.hashCode() : 0);
        return result;
    }
}
