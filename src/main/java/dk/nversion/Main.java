package dk.nversion;

import dk.nversion.record.RequestMessage;
import dk.nversion.record.RequestTest;

import java.nio.charset.StandardCharsets;

public class Main {

    public static void main(String[] args) throws Exception {
	    CopyBookSerializer requestTestSerializer = new CopyBookSerializer(RequestTest.class, CopyBookFormat.FULL, StandardCharsets.UTF_8);

        RequestTest test1 = new RequestTest().builder()
                .setId(1)
                .setCommand("cmd1234()")
                .setHello(new RequestMessage("Hello", "Body1234"))
                .setMessages(new RequestMessage[] { new RequestMessage("msg1", "stuff123"), new RequestMessage("msg2", "stuff12345") })
                .setArgs( new String[] { "do", "stuff" })
                .build();

        byte [] test1data = requestTestSerializer.serialize(test1);
        String test = new String(test1data, StandardCharsets.UTF_8);

        RequestTest test1result =  requestTestSerializer.deserialize(test1data, RequestTest.class);
        System.out.println(test1result);
    }
}
