package dk.nversion.copybook;

import dk.nversion.copybook.annotations.CopyBook;
import dk.nversion.copybook.annotations.CopyBookLine;

public class SerializerFieldValidationTest {

    @CopyBook()
    public class RightFieldTypeInteger {
        @CopyBookLine("01 FIELD PIC 9(8).")
        private int field;
    }

    @CopyBook()
    public class RightFieldTypeString {
        @CopyBookLine("01 FIELD PIC X(8).")
        private String field;
    }

    @CopyBook()
    public class RightFieldTypeDecimal {
        @CopyBookLine("01 FIELD PIC S9(16)V9(2).")
        private double field;
    }

    @CopyBook()
    public class RightFieldTypeIntArray {
        @CopyBookLine("01 FIELDS OCCURS 10 TIMES.")
        @CopyBookLine("02 FIELD PIC 9(8).")
        private int[] fields;
    }

    @CopyBook()
    public class RightFieldTypeStringArray {
        @CopyBookLine("01 FIELDS OCCURS 10 TIMES.")
        @CopyBookLine("02 FIELD PIC X(8).")
        private String[] fields;
    }

    @CopyBook()
    public class TestObject {
        private String field;
    }

    @CopyBook()
    public class RightFieldTypeObject {
        @CopyBookLine("01 OBJS.")
        private TestObject fields;
    }

    @CopyBook()
    public class RightFieldTypeObjectArray {
        @CopyBookLine("01 OBJS OCCURS 10 TIMES.")
        private TestObject[] fields;
    }

    @org.junit.Test
    public void testRightFieldTypeInteger() throws Exception {
        CopyBookSerializer requestTestSerializer = new CopyBookSerializer(RightFieldTypeInteger.class);
    }

    @org.junit.Test
    public void testRightFieldTypeString() throws Exception {
        CopyBookSerializer requestTestSerializer = new CopyBookSerializer(RightFieldTypeString.class);
    }

    @org.junit.Test
    public void testRightFieldTypeDecimal() throws Exception {
        CopyBookSerializer requestTestSerializer = new CopyBookSerializer(RightFieldTypeDecimal.class);
    }

    @org.junit.Test
    public void testRightFieldTypeObject() throws Exception {
        CopyBookSerializer requestTestSerializer = new CopyBookSerializer(RightFieldTypeObject.class);
    }

    @org.junit.Test
    public void testRightFieldTypeIntArray() throws Exception {
        CopyBookSerializer requestTestSerializer = new CopyBookSerializer(RightFieldTypeIntArray.class);
    }

    @org.junit.Test
    public void testRightFieldTypeStringArray() throws Exception {
        CopyBookSerializer requestTestSerializer = new CopyBookSerializer(RightFieldTypeStringArray.class);
    }

    @org.junit.Test
    public void testRightFieldTypeObjectArray() throws Exception {
        CopyBookSerializer requestTestSerializer = new CopyBookSerializer(RightFieldTypeObjectArray.class);
    }
}