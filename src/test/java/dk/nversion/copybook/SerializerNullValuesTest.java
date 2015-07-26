package dk.nversion.copybook;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

public class SerializerNullValuesTest {

    @CopyBook(format = CopyBookSerializationFormat.FULL)
    @CopyBookFieldFormat(fieldType = CopyBookFieldType.STRING, paddingChar = ' ', nullFillerChar = (byte)0, signingType = CopyBookFieldSigningType.PREFIX, rightPadding = false)
    static public class fieldTypeStringSetToNullFull {
        @CopyBookLine("01 FIELD PIC X(4).")
        public String field;
    }

    @CopyBook(format = CopyBookSerializationFormat.FULL)
    @CopyBookFieldFormat(fieldType = CopyBookFieldType.STRING, paddingChar = ' ', nullFillerChar = (byte)0, signingType = CopyBookFieldSigningType.PREFIX, rightPadding = false)
    static public class fieldTypeStringSetToNullPacked {
        @CopyBookLine("01 FIELD PIC X(4).")
        public String field;
    }

    @org.junit.Test
    public void testFieldTypeStringSetToNullFull() throws Exception {
        CopyBookSerializer serializer = new CopyBookSerializer(fieldTypeStringSetToNullFull.class);
        fieldTypeStringSetToNullFull test = new fieldTypeStringSetToNullFull();
        test.field = null;
        byte[] testBytes = serializer.serialize(test);
        assertArrayEquals(testBytes, new byte[]{ 0, 0, 0, 0 });
        fieldTypeStringSetToNullFull test2 = serializer.deserialize(testBytes, fieldTypeStringSetToNullFull.class);
        assertNull(test2.field);
    }

    @org.junit.Test
    public void testFieldTypeStringSetToNullPacked() throws Exception {
        CopyBookSerializer serializer = new CopyBookSerializer(fieldTypeStringSetToNullPacked.class);
        fieldTypeStringSetToNullPacked test = new fieldTypeStringSetToNullPacked();
        test.field = null;
        byte[] testBytes = serializer.serialize(test);
        assertArrayEquals(testBytes, new byte[]{ 0, 0, 0, 0 });
        fieldTypeStringSetToNullPacked test2 = serializer.deserialize(testBytes, fieldTypeStringSetToNullPacked.class);
        assertNull(test2.field);
    }
}