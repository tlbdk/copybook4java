package dk.nversion.old;

import dk.nversion.copybook.CopyBookSerializer;
import dk.nversion.copybook.annotations.CopyBook;
import dk.nversion.copybook.annotations.CopyBookLine;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;

public class SerializerPaddingTest {

    @CopyBook()
    static public class IntPadding {
        @CopyBookLine("01 FIELD PIC 9(4).")
        public int field;
    }

    @CopyBook()
    static public class StringPadding {
        @CopyBookLine("01 FIELD PIC X(4).")
        public String field;
    }

    @org.junit.Test
    public void testIntPadding() throws Exception {
        CopyBookSerializer serializer = new CopyBookSerializer(IntPadding.class);
        IntPadding test = new IntPadding();
        test.field = 0;
        byte[] testBytes = serializer.serialize(test);
        assertArrayEquals(testBytes, new byte[]{(byte) '0', (byte) '0', (byte) '0', (byte) '0'});
        IntPadding test2 = serializer.deserialize(testBytes, IntPadding.class);
        assertEquals(test.field, test2.field);
    }

    @org.junit.Test
    public void testStringPaddingEmptyString() throws Exception {
        CopyBookSerializer serializer = new CopyBookSerializer(StringPadding.class);
        StringPadding test = new StringPadding();
        test.field = "";
        byte[] testBytes = serializer.serialize(test);
        assertArrayEquals(testBytes, new byte[]{(byte) ' ', (byte) ' ', (byte) ' ', (byte) ' '});
        StringPadding test2 = serializer.deserialize(testBytes, StringPadding.class);
        assertEquals(test.field, test2.field);
    }

    @org.junit.Test
    public void testStringPaddingNullString() throws Exception {
        CopyBookSerializer serializer = new CopyBookSerializer(StringPadding.class);
        StringPadding test = new StringPadding();
        test.field = null;
        byte[] testBytes = serializer.serialize(test);
        assertArrayEquals(testBytes, new byte[]{(byte)0, (byte)0, (byte)0, (byte)0});
        StringPadding test2 = serializer.deserialize(testBytes, StringPadding.class);
        assertEquals(test.field, test2.field);
    }
}