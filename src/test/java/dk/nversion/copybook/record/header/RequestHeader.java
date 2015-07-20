package dk.nversion.copybook.record.header;

import dk.nversion.copybook.CopyBook;
import dk.nversion.copybook.CopyBookFieldType;
import dk.nversion.copybook.CopyBookSerializationFormat;
import dk.nversion.copybook.CopyBookFieldFormat;

@CopyBook(format = CopyBookSerializationFormat.FULL, charset = "UTF-8")
@CopyBookFieldFormat(fieldType = CopyBookFieldType.INT, rightPadding = true, paddingChar = '0')
@CopyBookFieldFormat(fieldType = CopyBookFieldType.STRING, rightPadding = true, paddingChar = ' ')
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
