package dk.nversion;

import dk.nversion.record.RequestMessage;
import dk.nversion.record.RequestTest;

import java.nio.ByteBuffer;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;

public class Main {

    public static void main(String[] args) throws Exception {
	    CopyBookSerializer requestTestSerializer = new CopyBookSerializer(RequestTest.class);

        RequestTest test1 = new RequestTest().builder()
                .setId(1)
                .setCommand("cmd1234()")
                .build();
        test1.hello = new RequestMessage("Hello", "Body1234");
        test1.messages = new RequestMessage[] { new RequestMessage("msg1", "stuff123"), new RequestMessage("msg2", "stuff12345") };
        test1.messages_count = test1.messages.length;
        test1.args = new String[] { "do", "stuff" };
        test1.args_count = test1.args.length;

        byte [] test1data = requestTestSerializer.serialize(test1, CopyBookOutputFormat.FULL, StandardCharsets.UTF_8);

        RequestTest test1result =  requestTestSerializer.deserialize(test1data, RequestTest.class);
    }
}
