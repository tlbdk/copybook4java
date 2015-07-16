package dk.nversion.copybook.record.header;

import dk.nversion.copybook.CopyBook;
import dk.nversion.copybook.CopyBookFieldType;
import dk.nversion.copybook.CopyBookFormat;
import dk.nversion.copybook.CopyBookPadding;

@CopyBook(format = CopyBookFormat.FULL, charset = "UTF-8")
@CopyBookPadding(fieldType = CopyBookFieldType.INT, right = true, character  = '0')
@CopyBookPadding(fieldType = CopyBookFieldType.STRING, right = true, character  = ' ')
public class RequestHeader {
    private String username;
    private String requestId;
    private String sessionID;

    // Builder
    public static RequestTestBuilder builder() {
        return new RequestTestBuilder();
    }

    public static final class RequestTestBuilder {
        private final RequestHeader requestHeader = new RequestHeader();

        public RequestTestBuilder setUsername(String username) {
            requestHeader.username = username;
            return this;
        }

        public RequestTestBuilder setRequestId(String id) {
            requestHeader.requestId = id;
            return this;
        }

        public RequestTestBuilder setSessionId(String id) {
            requestHeader.sessionID = id;
            return this;
        }

        public RequestHeader build() {
            return requestHeader;
        }
    }
}
