package dk.nversion.copybook.serializers;

import dk.nversion.copybook.CopyBookSerializer;
import dk.nversion.copybook.annotations.CopyBook;
import dk.nversion.copybook.annotations.CopyBookLine;

import java.math.BigDecimal;

import static junit.framework.TestCase.assertEquals;
import static org.junit.Assert.assertArrayEquals;

public class FullLastArrayShortTest {
    @CopyBook(type = FullLastArrayShort.class)
    static public class StringFieldOccursTwoTimes {
        @CopyBookLine("01 COUNT PIC 9.")
        public int fields_count;
        @CopyBookLine("01 FIELDS OCCURS 10 TIMES PIC X(4).")
        public String[] fields;
    }

    @org.junit.Test
    public void testStringFieldOccursTwoTimes() throws Exception {
        CopyBookSerializer serializer = new CopyBookSerializer(StringFieldOccursTwoTimes.class);
        StringFieldOccursTwoTimes test = new StringFieldOccursTwoTimes();
        test.fields = new String[] {"abcd", "1234"};
        test.fields_count = test.fields.length;
        byte[] testBytes = serializer.serialize(test);
        assertEquals(1 + 4 * test.fields_count, testBytes.length);
        StringFieldOccursTwoTimes test2 = serializer.deserialize(testBytes, StringFieldOccursTwoTimes.class);
        assertArrayEquals(test.fields, test2.fields);
    }

    @CopyBook(type = FullLastArrayShort.class)
    static public class IntFieldOccursTwoTimes {
        @CopyBookLine("01 COUNT PIC 9.")
        public int fields_count;
        @CopyBookLine("01 FIELDS OCCURS 10 TIMES PIC 9(4).")
        public int[] fields;
    }

    @org.junit.Test
    public void testIntFieldOccursTwoTimes() throws Exception {
        CopyBookSerializer serializer = new CopyBookSerializer(IntFieldOccursTwoTimes.class);
        IntFieldOccursTwoTimes test = new IntFieldOccursTwoTimes();
        test.fields = new int[] {1, 2};
        test.fields_count = test.fields.length;
        byte[] testBytes = serializer.serialize(test);
        assertEquals(1 + 4 * test.fields_count, testBytes.length);
        IntFieldOccursTwoTimes test2 = serializer.deserialize(testBytes, IntFieldOccursTwoTimes.class);
        assertArrayEquals(test.fields, test2.fields);
    }

    @CopyBook(type = FullLastArrayShort.class)
    static public class SignedIntFieldOccursTwoTimes {
        @CopyBookLine("01 COUNT PIC 9.")
        public int fields_count;
        @CopyBookLine("01 FIELDS OCCURS 10 TIMES PIC S9(4).")
        public int[] fields;
    }

    @org.junit.Test
    public void testSignedIntFieldOccursTwoTimes() throws Exception {
        CopyBookSerializer serializer = new CopyBookSerializer(SignedIntFieldOccursTwoTimes.class);
        SignedIntFieldOccursTwoTimes test = new SignedIntFieldOccursTwoTimes();
        test.fields = new int[] {-1, -2};
        test.fields_count = test.fields.length;
        byte[] testBytes = serializer.serialize(test);
        assertEquals(1 + 4 * test.fields_count, testBytes.length);
        SignedIntFieldOccursTwoTimes test2 = serializer.deserialize(testBytes, SignedIntFieldOccursTwoTimes.class);
        assertArrayEquals(test.fields, test2.fields);
    }

    @CopyBook(type = FullLastArrayShort.class)
    static public class DecimalFieldOccursTwoTimes {
        @CopyBookLine("01 COUNT PIC 9.")
        public int fields_count;
        @CopyBookLine("01 FIELDS OCCURS 10 TIMES PIC 9(3)V9(2).")
        public BigDecimal[] fields;
    }

    @org.junit.Test
    public void testDecimalFieldOccursTwoTimes() throws Exception {
        CopyBookSerializer serializer = new CopyBookSerializer(DecimalFieldOccursTwoTimes.class);
        DecimalFieldOccursTwoTimes test = new DecimalFieldOccursTwoTimes();
        test.fields = new BigDecimal[] {new BigDecimal("1.00"), new BigDecimal("2.00")};
        test.fields_count = test.fields.length;
        byte[] testBytes = serializer.serialize(test);
        assertEquals(1 + 5 * test.fields_count, testBytes.length);
        DecimalFieldOccursTwoTimes test2 = serializer.deserialize(testBytes, DecimalFieldOccursTwoTimes.class);
        assertArrayEquals(test.fields, test2.fields);
    }

    @CopyBook(type = FullLastArrayShort.class)
    static public class SignedDecimalFieldOccursTwoTimes {
        @CopyBookLine("01 COUNT PIC 9.")
        public int fields_count;
        @CopyBookLine("01 FIELDS OCCURS 10 TIMES PIC S9(3)V9(2).")
        public BigDecimal[] fields;
    }

    @org.junit.Test
    public void testSignedDecimalFieldOccursTwoTimes() throws Exception {
        CopyBookSerializer serializer = new CopyBookSerializer(SignedDecimalFieldOccursTwoTimes.class);
        SignedDecimalFieldOccursTwoTimes test = new SignedDecimalFieldOccursTwoTimes();
        test.fields = new BigDecimal[] {new BigDecimal("-1.00"), new BigDecimal("-2.00")};
        test.fields_count = test.fields.length;
        byte[] testBytes = serializer.serialize(test);
        assertEquals(1 + 5 * test.fields_count, testBytes.length);
        SignedDecimalFieldOccursTwoTimes test2 = serializer.deserialize(testBytes, SignedDecimalFieldOccursTwoTimes.class);
        assertArrayEquals(test.fields, test2.fields);
    }
}
