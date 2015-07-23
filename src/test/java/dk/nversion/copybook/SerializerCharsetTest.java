package dk.nversion.copybook;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;

public class SerializerCharsetTest {

    @CopyBook(charset = "cp037")
    @CopyBookFieldFormat(fieldType = CopyBookFieldType.STRING, paddingChar = ' ', nullFillerChar = (byte)0, signingType = CopyBookFieldSigningType.PREFIX, rightPadding = false)
    static public class fieldTypeStringCp037 {
        @CopyBookLine("01 FIELD PIC X(4).")
        public String field;
    }

    @org.junit.Test
    public void testFieldTypeStringCp037() throws Exception {
        CopyBookSerializer serializer = new CopyBookSerializer(fieldTypeStringCp037.class);
        fieldTypeStringCp037 test = new fieldTypeStringCp037();
        test.field = "ok";
        byte[] testBytes = serializer.serialize(test);
        assertArrayEquals(testBytes, new String("  ok").getBytes("cp037"));
        fieldTypeStringCp037 test2 = serializer.deserialize(testBytes, fieldTypeStringCp037.class);
        assertEquals("ok", test2.field);
    }
}