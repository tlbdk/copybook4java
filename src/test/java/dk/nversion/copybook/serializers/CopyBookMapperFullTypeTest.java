package dk.nversion.copybook.serializers;

import dk.nversion.copybook.CopyBookSerializer;
import dk.nversion.copybook.annotations.CopyBook;
import dk.nversion.copybook.annotations.CopyBookFieldFormat;
import dk.nversion.copybook.annotations.CopyBookLine;
import dk.nversion.copybook.converters.SignedDecimalToBigDecimal;
import dk.nversion.copybook.converters.SignedIntegerToInteger;
import dk.nversion.copybook.converters.TypeConverterIntEnum;
import dk.nversion.copybook.converters.TypeConverterStringEnum;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.nio.charset.StandardCharsets;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;

public class CopyBookMapperFullTypeTest {
    @CopyBook(type = FullMapper.class)
    static public class fieldTypeIntegerToInt {
        @CopyBookLine("01 FIELD PIC 9(2).")
        public int field;
    }

    @org.junit.Test
    public void testFieldTypeUnsignedIntegerToInt() throws Exception {
        CopyBookSerializer serializer = new CopyBookSerializer(fieldTypeIntegerToInt.class);
        fieldTypeIntegerToInt test = new fieldTypeIntegerToInt();
        test.field = 10;
        byte[] testBytes = serializer.serialize(test);
        assertArrayEquals(testBytes, new byte[]{(byte) '1', (byte) '0'});
        fieldTypeIntegerToInt test2 = serializer.deserialize(testBytes, fieldTypeIntegerToInt.class);
        assertEquals(10, test2.field);
    }

    @CopyBook(type = FullMapper.class)
    static public class fieldTypeSignedIntegerToInt {
        @CopyBookLine("01 FIELD PIC S9(3).")
        public int field;
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

    @CopyBook(type = FullMapper.class)
    static public class fieldTypeUnsignedIntegerToLong {
        @CopyBookLine("01 FIELD PIC 9(2).")
        public long field;
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

    @CopyBook(type = FullMapper.class)
    static public class fieldTypeSignedIntegerToLong {
        @CopyBookLine("01 FIELD PIC S9(3).")
        public long field;
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

    @CopyBook(type = FullMapper.class)
    static public class fieldTypeUnsignedIntegerToBigInteger {
        @CopyBookLine("01 FIELD PIC 9(2).")
        public BigInteger field;
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

    @CopyBook(type = FullMapper.class)
    static public class fieldTypeSignedIntegerToBigInteger {
        @CopyBookLine("01 FIELD PIC S9(3).")
        public BigInteger field;
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

    @CopyBook(type = FullMapper.class)
    static public class fieldTypeString {
        @CopyBookLine("01 FIELD PIC X(2).")
        public String field;
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

    @CopyBook(type = FullMapper.class)
    static public class fieldTypeUnsignedDecimalToBigDecimal {
        @CopyBookLine("01 FIELD PIC 9(2)V9(2).")
        public BigDecimal field;
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

    @CopyBook(type = FullMapper.class)
    static public class fieldTypeSignedDecimalToBigDecimal {
        @CopyBookLine("01 FIELD PIC S9(3)V9(2).")
        public BigDecimal field;
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

    @CopyBook(type = FullMapper.class)
    @CopyBookFieldFormat(type = SignedIntegerToInteger.class, paddingChar = '0', nullFillerChar = (byte)0, signingType = CopyBookFieldSigningType.POSTFIX, rightPadding = false)
    static public class fieldTypeSignedIntegerPostfix {
        @CopyBookLine("01 FIELD PIC S9(3).")
        public int field;
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

    @CopyBook(type = FullMapper.class)
    @CopyBookFieldFormat(type = SignedDecimalToBigDecimal.class, paddingChar = '0', nullFillerChar = (byte)0, signingType = CopyBookFieldSigningType.POSTFIX, rightPadding = false)
    static public class fieldTypeSignedDecimalPostfix {
        @CopyBookLine("01 FIELD PIC S9(3)V9(2).")
        public BigDecimal field;
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

    @CopyBook(type = FullMapper.class)
    @CopyBookFieldFormat(type = SignedIntegerToInteger.class, paddingChar = '0', nullFillerChar = (byte)0, signingType = CopyBookFieldSigningType.LAST_BYTE_BIT8, rightPadding = false)
    static public class fieldTypeSignedIntegerToIntWithEmbeddedSigningLastByteBit8 {
        @CopyBookLine("01 FIELD PIC S9(2).")
        public int field;
    }

    @org.junit.Test
    public void testFieldTypeSignedIntegerToIntWithEmbeddedSigningLastByteBit8() throws Exception {
        CopyBookSerializer serializer = new CopyBookSerializer(fieldTypeSignedIntegerToIntWithEmbeddedSigningLastByteBit8.class);
        fieldTypeSignedIntegerToIntWithEmbeddedSigningLastByteBit8 test = new fieldTypeSignedIntegerToIntWithEmbeddedSigningLastByteBit8();
        test.field = -10;
        byte[] testBytes = serializer.serialize(test);
        assertArrayEquals(testBytes, new byte[] { (byte)'1', (byte)-80 } );
        fieldTypeSignedIntegerToIntWithEmbeddedSigningLastByteBit8 test2 = serializer.deserialize(testBytes, fieldTypeSignedIntegerToIntWithEmbeddedSigningLastByteBit8.class);
        assertEquals(-10, test2.field);
    }

    @CopyBook(charset = "cp037", type = FullMapper.class)
    @CopyBookFieldFormat(type = SignedIntegerToInteger.class, paddingChar = '0', nullFillerChar = (byte)0, signingType = CopyBookFieldSigningType.LAST_BYTE_EBCDIC_BIT5, rightPadding = false)
    static public class fieldTypeSignedIntegerToIntWithEmbeddedSigningLastByteEbcdicBit5 {
        @CopyBookLine("01 FIELD PIC S9(2).")
        public int field;
    }

    @org.junit.Test
    public void testFieldTypeSignedIntegerToIntWithEmbeddedSigningLastByteEbcdicBit5() throws Exception {
        CopyBookSerializer serializer = new CopyBookSerializer(fieldTypeSignedIntegerToIntWithEmbeddedSigningLastByteEbcdicBit5.class);
        fieldTypeSignedIntegerToIntWithEmbeddedSigningLastByteEbcdicBit5 test = new fieldTypeSignedIntegerToIntWithEmbeddedSigningLastByteEbcdicBit5();
        test.field = -10;
        byte[] testBytes = serializer.serialize(test);
        assertArrayEquals(testBytes, new byte[]{(byte) -15, (byte) -48});
        fieldTypeSignedIntegerToIntWithEmbeddedSigningLastByteEbcdicBit5 test2 = serializer.deserialize(testBytes, fieldTypeSignedIntegerToIntWithEmbeddedSigningLastByteEbcdicBit5.class);
        assertEquals(-10, test2.field);

        // Test with some more values that should fit into the fields
        for(int i=-99; i < 99; i++) {
            test.field = i;
            testBytes = serializer.serialize(test);
            test2 = serializer.deserialize(testBytes, fieldTypeSignedIntegerToIntWithEmbeddedSigningLastByteEbcdicBit5.class);
            assertEquals(i, test2.field);
        }
    }

    public enum TestIntEnum implements TypeConverterIntEnum {
        HIGH(30), MEDIUM(20), LOW(10);
        private final int value;

        TestIntEnum(int value) {
            this.value = value;
        }

        public int getValue() {
            return value;
        }
    }

    @CopyBook(type = FullMapper.class)
    static public class fieldTypeIntegerToIntegerToTypeConverterIntEnum {
        @CopyBookLine("01 FIELD PIC 9(2).")
        public TestIntEnum field;
    }

    @org.junit.Test
    public void testfieldTypeIntegerToIntegerToTypeConverterIntEnum() throws Exception {
        CopyBookSerializer serializer = new CopyBookSerializer(fieldTypeIntegerToIntegerToTypeConverterIntEnum.class);
        fieldTypeIntegerToIntegerToTypeConverterIntEnum test = new fieldTypeIntegerToIntegerToTypeConverterIntEnum();
        test.field = TestIntEnum.HIGH;
        byte[] testBytes = serializer.serialize(test);
        assertArrayEquals(testBytes, new byte[]{(byte) '3', (byte) '0'});
        fieldTypeIntegerToIntegerToTypeConverterIntEnum test2 = serializer.deserialize(testBytes, fieldTypeIntegerToIntegerToTypeConverterIntEnum.class);
        assertEquals(TestIntEnum.HIGH, test2.field);
    }

    public enum TestStringEnum implements TypeConverterStringEnum {
        HIGH("HH"), MEDIUM("MM"), LOW("LL");
        private final String value;

        TestStringEnum(String value) {
            this.value = value;
        }

        public String getValue() {
            return value;
        }
    }

    @CopyBook(type = FullMapper.class)
    static public class fieldTypeIntegerToIntegerToTypeConverterStringEnum {
        @CopyBookLine("01 FIELD PIC X(2).")
        public TestStringEnum field;
    }

    @org.junit.Test
    public void testFieldTypeIntegerToIntegerToTypeConverterStringEnum() throws Exception {
        CopyBookSerializer serializer = new CopyBookSerializer(fieldTypeIntegerToIntegerToTypeConverterStringEnum.class);
        fieldTypeIntegerToIntegerToTypeConverterStringEnum test = new fieldTypeIntegerToIntegerToTypeConverterStringEnum();
        test.field = TestStringEnum.HIGH;
        byte[] testBytes = serializer.serialize(test);
        assertArrayEquals(testBytes, "HH".getBytes(StandardCharsets.UTF_8));
        fieldTypeIntegerToIntegerToTypeConverterStringEnum test2 = serializer.deserialize(testBytes, fieldTypeIntegerToIntegerToTypeConverterStringEnum.class);
        assertEquals(TestStringEnum.HIGH, test2.field);
    }

    @CopyBook(type = FullMapper.class)
    static public class fieldTypeIntegerToBoolean {
        @CopyBookLine("01 FIELD PIC 9(2).")
        public boolean field;
    }

    @org.junit.Test
    public void testFieldTypeIntegerToBoolean() throws Exception {
        CopyBookSerializer serializer = new CopyBookSerializer(fieldTypeIntegerToBoolean.class);
        fieldTypeIntegerToBoolean test = new fieldTypeIntegerToBoolean();
        test.field = true;
        byte[] testBytes = serializer.serialize(test);
        assertArrayEquals(testBytes, new byte[]{(byte) '0', (byte) '1'});
        fieldTypeIntegerToBoolean test2 = serializer.deserialize(testBytes, fieldTypeIntegerToBoolean.class);
        assertEquals(true, test2.field);
    }

    @CopyBook(type = FullMapper.class)
    static public class fieldTypeSignedIntegerToBoolean {
        @CopyBookLine("01 FIELD PIC 9(2).")
        public boolean field;
    }

    @org.junit.Test
    public void testfieldTypeSignedIntegerToBoolean() throws Exception {
        CopyBookSerializer serializer = new CopyBookSerializer(fieldTypeSignedIntegerToBoolean.class);
        fieldTypeSignedIntegerToBoolean test = new fieldTypeSignedIntegerToBoolean();
        test.field = true;
        byte[] testBytes = serializer.serialize(test);
        assertArrayEquals(testBytes, new byte[]{(byte) '0', (byte) '1'});
        fieldTypeSignedIntegerToBoolean test2 = serializer.deserialize(testBytes, fieldTypeSignedIntegerToBoolean.class);
        assertEquals(true, test2.field);
    }

    @CopyBook(type = FullMapper.class)
    static public class fieldTypeStringToBoolean {
        @CopyBookLine("01 FIELD PIC X(2).")
        public boolean field;
    }

    @org.junit.Test
    public void testFieldTypeStringToBoolean() throws Exception {
        CopyBookSerializer serializer = new CopyBookSerializer(fieldTypeStringToBoolean.class);
        fieldTypeStringToBoolean test = new fieldTypeStringToBoolean();
        test.field = true;
        byte[] testBytes = serializer.serialize(test);
        assertArrayEquals(testBytes, "Y ".getBytes(StandardCharsets.UTF_8));
        fieldTypeStringToBoolean test2 = serializer.deserialize(testBytes, fieldTypeStringToBoolean.class);
        assertEquals(true, test2.field);
    }

}