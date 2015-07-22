package dk.nversion.copybook;

import java.math.BigDecimal;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;

public class SerializerTypeTest {

    @CopyBook()
    static public class rightFieldTypeInteger {
        @CopyBookLine("01 FIELD PIC 9(2).")
        public int field;
    }

    @CopyBook()
    static public class rightFieldTypeString {
        @CopyBookLine("01 FIELD PIC X(2).")
        public String field;
    }

    @CopyBook()
    static public class rightFieldTypeDecimal {
        @CopyBookLine("01 FIELD PIC 9(2)V9(2).")
        public BigDecimal field;
    }

    @org.junit.Test
    public void testRightFieldTypeInteger() throws Exception {
        CopyBookSerializer serializer = new CopyBookSerializer(rightFieldTypeInteger.class);
        rightFieldTypeInteger test = new rightFieldTypeInteger();
        test.field = 10;
        byte[] testBytes = serializer.serialize(test);
        assertArrayEquals(testBytes, new byte[]{(byte) '1', (byte) '0'});
        rightFieldTypeInteger test2 = serializer.deserialize(testBytes, rightFieldTypeInteger.class);
        assertEquals(10, test2.field);
    }

    @org.junit.Test
    public void testRightFieldTypeString() throws Exception {
        CopyBookSerializer serializer = new CopyBookSerializer(rightFieldTypeString.class);
        rightFieldTypeString test = new rightFieldTypeString();
        test.field = "ok";
        byte[] testBytes = serializer.serialize(test);
        assertArrayEquals(testBytes, new byte[] { (byte)'o', (byte)'k'});
        rightFieldTypeString test2 = serializer.deserialize(testBytes, rightFieldTypeString.class);
        assertEquals("ok", test2.field);
    }

    @org.junit.Test
    public void testRightFieldTypeDecimal() throws Exception {
        CopyBookSerializer serializer = new CopyBookSerializer(rightFieldTypeDecimal.class);
        rightFieldTypeDecimal test = new rightFieldTypeDecimal();
        test.field = new BigDecimal("10.01");
        byte[] testBytes = serializer.serialize(test);
        assertArrayEquals(testBytes, new byte[] { (byte)'1', (byte)'0', (byte)'0', (byte)'1'});
        rightFieldTypeDecimal test2 = serializer.deserialize(testBytes, rightFieldTypeDecimal.class);
        assertEquals(new BigDecimal("10.01"), test2.field);
    }
}