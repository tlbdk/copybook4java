package dk.nversion.copybook;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

public class SerializerNullValuesTest {

    @CopyBook()
    @CopyBookFieldFormat(fieldType = CopyBookFieldType.STRING, paddingChar = ' ', nullFillerChar = (byte)0, signingPostfix = true, rightPadding = false)
    static public class fieldTypeStringSetToNull {
        @CopyBookLine("01 FIELD PIC X(4).")
        public String field;
    }

    @org.junit.Test
    public void testFieldTypeStringSetToNull() throws Exception {
        CopyBookSerializer serializer = new CopyBookSerializer(fieldTypeStringSetToNull.class);
        fieldTypeStringSetToNull test = new fieldTypeStringSetToNull();
        test.field = null;
        byte[] testBytes = serializer.serialize(test);
        assertArrayEquals(testBytes, new byte[]{ 0, 0, 0, 0 });
        fieldTypeStringSetToNull test2 = serializer.deserialize(testBytes, fieldTypeStringSetToNull.class);
        assertNull(test2.field);
    }
}