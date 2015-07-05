package dk.nversion;

import dk.nversion.record.RequestMessage;
import dk.nversion.record.RequestTest;

public class Main {

    public static void main(String[] args) throws Exception {
	    CopyBookSerializer serializer = new CopyBookSerializer();

        RequestTest test1 = new RequestTest();
        test1.id = 1;
        test1.command = "cmd1234()";
        test1.hello = new RequestMessage("Hello", "Body1234");
        test1.messages = new RequestMessage[] { new RequestMessage("msg1", "stuff123"), new RequestMessage("msg1", "stuff123") };
        test1.messages_count = test1.messages.length;
        test1.args = new String[] { "do", "stuff" };
        test1.args_count = test1.args.length;

        byte [] test1data = new byte[8000];
        serializer.serialize(test1, test1data, 0);

        RequestTest test1result =  serializer.deserialize(test1data, RequestTest.class);
    }
}
