package dk.nversion.copybook;

import java.math.BigDecimal;
import java.math.BigInteger;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;

public class SerializerTypeTest {

    @CopyBook()
    static public class fieldTypeUnsignedIntegerToInt {
        @CopyBookLine("01 FIELD PIC 9(2).")
        public int field;
    }

    @CopyBook()
    static public class fieldTypeSignedIntegerToInt {
        @CopyBookLine("01 FIELD PIC S9(3).")
        public int field;
    }

    @CopyBook()
    static public class fieldTypeUnsignedIntegerToLong {
        @CopyBookLine("01 FIELD PIC 9(2).")
        public long field;
    }

    @CopyBook()
    static public class fieldTypeSignedIntegerToLong {
        @CopyBookLine("01 FIELD PIC S9(3).")
        public long field;
    }

    @CopyBook()
    static public class fieldTypeUnsignedIntegerToBigInteger {
        @CopyBookLine("01 FIELD PIC 9(2).")
        public BigInteger field;
    }

    @CopyBook()
    static public class fieldTypeSignedIntegerToBigInteger {
        @CopyBookLine("01 FIELD PIC S9(3).")
        public BigInteger field;
    }

    @CopyBook()
    static public class fieldTypeString {
        @CopyBookLine("01 FIELD PIC X(2).")
        public String field;
    }

    @CopyBook()
    static public class fieldTypeUnsignedDecimalToBigDecimal {
        @CopyBookLine("01 FIELD PIC 9(2)V9(2).")
        public BigDecimal field;
    }

    @CopyBook()
    static public class fieldTypeSignedDecimalToBigDecimal {
        @CopyBookLine("01 FIELD PIC S9(3)V9(2).")
        public BigDecimal field;
    }

    @CopyBook()
    @CopyBookFieldFormat(fieldType = CopyBookFieldType.SIGNED_INT, paddingChar = '0', nullFillerChar = (byte)0, signingPostfix = true, rightPadding = false)
    static public class fieldTypeSignedIntegerPostfix {
        @CopyBookLine("01 FIELD PIC S9(3).")
        public int field;
    }

    @CopyBook()
    @CopyBookFieldFormat(fieldType = CopyBookFieldType.SIGNED_DECIMAL, paddingChar = '0', nullFillerChar = (byte)0, signingPostfix = true, rightPadding = false)
    static public class fieldTypeSignedDecimalPostfix {
        @CopyBookLine("01 FIELD PIC S9(3)V9(2).")
        public BigDecimal field;
    }


    @org.junit.Test
    public void testFieldTypeUnsignedIntegerToInt() throws Exception {
        CopyBookSerializer serializer = new CopyBookSerializer(fieldTypeUnsignedIntegerToInt.class);
        fieldTypeUnsignedIntegerToInt test = new fieldTypeUnsignedIntegerToInt();
        test.field = 10;
        byte[] testBytes = serializer.serialize(test);
        assertArrayEquals(testBytes, new byte[]{(byte) '1', (byte) '0'});
        fieldTypeUnsignedIntegerToInt test2 = serializer.deserialize(testBytes, fieldTypeUnsignedIntegerToInt.class);
        assertEquals(10, test2.field);
    }

    @org.junit.Test
    public void testFieldTypeSignedIntegerToInt() throws Exception {
        CopyBookSerializer serializer = new CopyBookSerializer(fieldTypeSignedIntegerToInt.class);
        fieldTypeSignedIntegerToInt test = new fieldTypeSignedIntegerToInt();
        test.field = -10;
        byte[] testBytes = serializer.serialize(test);
        assertArrayEquals(testBytes, new byte[]{(byte) '-', (byte) '1', (byte) '0'});
        fieldTypeSignedIntegerToInt test2 = serializer.deserialize(testBytes, fieldTypeSignedIntegerToInt.class);
        assertEquals(-10, test2.field);
    }

    @org.junit.Test
    public void testFieldTypeUnsignedIntegerToLong() throws Exception {
        CopyBookSerializer serializer = new CopyBookSerializer(fieldTypeUnsignedIntegerToLong.class);
        fieldTypeUnsignedIntegerToLong test = new fieldTypeUnsignedIntegerToLong();
        test.field = 10L;
        byte[] testBytes = serializer.serialize(test);
        assertArrayEquals(testBytes, new byte[]{(byte) '1', (byte) '0'});
        fieldTypeUnsignedIntegerToLong test2 = serializer.deserialize(testBytes, fieldTypeUnsignedIntegerToLong.class);
        assertEquals(10L, test2.field);
    }

    @org.junit.Test
    public void testFieldTypeSignedIntegerToLong() throws Exception {
        CopyBookSerializer serializer = new CopyBookSerializer(fieldTypeSignedIntegerToLong.class);
        fieldTypeSignedIntegerToLong test = new fieldTypeSignedIntegerToLong();
        test.field = -10L;
        byte[] testBytes = serializer.serialize(test);
        assertArrayEquals(testBytes, new byte[]{(byte) '-', (byte) '1', (byte) '0'});
        fieldTypeSignedIntegerToLong test2 = serializer.deserialize(testBytes, fieldTypeSignedIntegerToLong.class);
        assertEquals(-10L, test2.field);
    }

    @org.junit.Test
    public void testFieldTypeUnsignedIntegerToBigInteger() throws Exception {
        CopyBookSerializer serializer = new CopyBookSerializer(fieldTypeUnsignedIntegerToBigInteger.class);
        fieldTypeUnsignedIntegerToBigInteger test = new fieldTypeUnsignedIntegerToBigInteger();
        test.field = new BigInteger("10");
        byte[] testBytes = serializer.serialize(test);
        assertArrayEquals(testBytes, new byte[]{(byte) '1', (byte) '0'});
        fieldTypeUnsignedIntegerToBigInteger test2 = serializer.deserialize(testBytes, fieldTypeUnsignedIntegerToBigInteger.class);
        assertEquals(new BigInteger("10"), test2.field);
    }

    @org.junit.Test
    public void testFieldTypeSignedIntegerToBigInteger() throws Exception {
        CopyBookSerializer serializer = new CopyBookSerializer(fieldTypeSignedIntegerToBigInteger.class);
        fieldTypeSignedIntegerToBigInteger test = new fieldTypeSignedIntegerToBigInteger();
        test.field = new BigInteger("-10");
        byte[] testBytes = serializer.serialize(test);
        assertArrayEquals(testBytes, new byte[]{(byte) '-', (byte) '1', (byte) '0'});
        fieldTypeSignedIntegerToBigInteger test2 = serializer.deserialize(testBytes, fieldTypeSignedIntegerToBigInteger.class);
        assertEquals(new BigInteger("-10"), test2.field);
    }


    @org.junit.Test
    public void testFieldTypeString() throws Exception {
        CopyBookSerializer serializer = new CopyBookSerializer(fieldTypeString.class);
        fieldTypeString test = new fieldTypeString();
        test.field = "ok";
        byte[] testBytes = serializer.serialize(test);
        assertArrayEquals(testBytes, new byte[] { (byte)'o', (byte)'k'});
        fieldTypeString test2 = serializer.deserialize(testBytes, fieldTypeString.class);
        assertEquals("ok", test2.field);
    }

    @org.junit.Test
    public void testFieldTypeUnsignedDecimal() throws Exception {
        CopyBookSerializer serializer = new CopyBookSerializer(fieldTypeUnsignedDecimalToBigDecimal.class);
        fieldTypeUnsignedDecimalToBigDecimal test = new fieldTypeUnsignedDecimalToBigDecimal();
        test.field = new BigDecimal("10.01");
        byte[] testBytes = serializer.serialize(test);
        assertArrayEquals(testBytes, new byte[] { (byte)'1', (byte)'0', (byte)'0', (byte)'1'});
        fieldTypeUnsignedDecimalToBigDecimal test2 = serializer.deserialize(testBytes, fieldTypeUnsignedDecimalToBigDecimal.class);
        assertEquals(new BigDecimal("10.01"), test2.field);
    }

    @org.junit.Test
    public void testFieldTypeSignedDecimal() throws Exception {
        CopyBookSerializer serializer = new CopyBookSerializer(fieldTypeSignedDecimalToBigDecimal.class);
        fieldTypeSignedDecimalToBigDecimal test = new fieldTypeSignedDecimalToBigDecimal();
        test.field = new BigDecimal("-10.01");
        byte[] testBytes = serializer.serialize(test);
        assertArrayEquals(testBytes, new byte[] { (byte)'-', (byte)'1', (byte)'0', (byte)'0', (byte)'1'});
        fieldTypeSignedDecimalToBigDecimal test2 = serializer.deserialize(testBytes, fieldTypeSignedDecimalToBigDecimal.class);
        assertEquals(new BigDecimal("-10.01"), test2.field);
    }

    @org.junit.Test
    public void testFieldTypeSignedIntegerPostfix() throws Exception {
        CopyBookSerializer serializer = new CopyBookSerializer(fieldTypeSignedIntegerPostfix.class);
        fieldTypeSignedIntegerPostfix test = new fieldTypeSignedIntegerPostfix();
        test.field = -10;
        byte[] testBytes = serializer.serialize(test);
        assertArrayEquals(testBytes, new byte[]{ (byte) '1', (byte) '0', (byte) '-'});
        fieldTypeSignedIntegerPostfix test2 = serializer.deserialize(testBytes, fieldTypeSignedIntegerPostfix.class);
        assertEquals(-10, test2.field);
    }

    @org.junit.Test
    public void testFieldTypeSignedDecimalPostfix() throws Exception {
        CopyBookSerializer serializer = new CopyBookSerializer(fieldTypeSignedDecimalPostfix.class);
        fieldTypeSignedDecimalPostfix test = new fieldTypeSignedDecimalPostfix();
        test.field = new BigDecimal("-10.01");
        byte[] testBytes = serializer.serialize(test);
        assertArrayEquals(testBytes, new byte[] { (byte)'1', (byte)'0', (byte)'0', (byte)'1', (byte)'-' } );
        fieldTypeSignedDecimalPostfix test2 = serializer.deserialize(testBytes, fieldTypeSignedDecimalPostfix.class);
        assertEquals(new BigDecimal("-10.01"), test2.field);
    }
}