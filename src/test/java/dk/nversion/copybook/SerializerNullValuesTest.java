package dk.nversion.copybook;

import dk.nversion.copybook.annotations.CopyBook;
import dk.nversion.copybook.annotations.CopyBookFieldFormat;
import dk.nversion.copybook.annotations.CopyBookLine;
import dk.nversion.copybook.converters.StringToString;
import dk.nversion.copybook.serializers.FullSerializer;
import dk.nversion.copybook.serializers.PackedFirstLevelSerializer;

import static org.junit.Assert.*;

public class SerializerNullValuesTest {

    @CopyBook(type = FullSerializer.class)
    @CopyBookFieldFormat(type = StringToString.class, paddingChar = ' ', nullFillerChar = (byte)0, signingType = CopyBookFieldSigningType.PREFIX, rightPadding = false)
    static public class fieldTypeStringSetToNullFull {
        @CopyBookLine("01 FIELD PIC X(4).")
        public String field;
    }

    @CopyBook(type = PackedFirstLevelSerializer.class)
    @CopyBookFieldFormat(type = StringToString.class, paddingChar = ' ', nullFillerChar = (byte)0, signingType = CopyBookFieldSigningType.PREFIX, rightPadding = false)
    static public class fieldTypeStringSetToNullPacked {
        @CopyBookLine("01 FIELD PIC X(4).")
        public String field;
    }

    @CopyBook()
    static public class objectField {
        @CopyBookLine("01 FIELD PIC X(4).")
        public String field;
    }

    @CopyBook(type = PackedFirstLevelSerializer.class)
    @CopyBookFieldFormat(type = StringToString.class, paddingChar = ' ', nullFillerChar = (byte)0, signingType = CopyBookFieldSigningType.PREFIX, rightPadding = false)
    static public class fieldTypeNestedNullPacked {
        @CopyBookLine("01 FIELD.")
        public objectField field;
    }

    @CopyBook()
    static public class objectFieldArray {
        @CopyBookLine("01 FIELDS OCCURS 2 TIMES.")
        @CopyBookLine("01 FIELD PIC X(2).")
        public String[] fields;
    }

    @CopyBook(type = PackedFirstLevelSerializer.class)
    @CopyBookFieldFormat(type = StringToString.class, paddingChar = ' ', nullFillerChar = (byte)0, signingType = CopyBookFieldSigningType.PREFIX, rightPadding = false)
    static public class fieldTypeNestedArrayNullPacked {
        @CopyBookLine("01 FIELD.")
        public objectFieldArray field;
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
        assertArrayEquals(testBytes, new byte[]{ 0, 0, 0, 0, 0, 0, 0, 0 });
        fieldTypeStringSetToNullPacked test2 = serializer.deserialize(testBytes, fieldTypeStringSetToNullPacked.class);
        assertNull(test2.field);
    }

    @org.junit.Test
    public void testFieldTypeNestedNullPacked() throws Exception {
        CopyBookSerializer serializer = new CopyBookSerializer(fieldTypeNestedNullPacked.class);
        fieldTypeNestedNullPacked test = new fieldTypeNestedNullPacked();
        test.field = new objectField();
        test.field.field = null;
        byte[] testBytes = serializer.serialize(test);
        assertArrayEquals(testBytes, new byte[]{-128, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 11});
        fieldTypeNestedNullPacked test2 = serializer.deserialize(testBytes, fieldTypeNestedNullPacked.class);
        assertNotNull(test2.field);
        assertNull(test2.field.field);
    }

    @org.junit.Test
    public void testFieldTypeNestedArrayNullPacked() throws Exception {
        CopyBookSerializer serializer = new CopyBookSerializer(fieldTypeNestedArrayNullPacked.class);
        fieldTypeNestedArrayNullPacked test = new fieldTypeNestedArrayNullPacked();
        test.field = new objectFieldArray();
        test.field.fields = new String[] { "do", null };
        byte[] testBytes = serializer.serialize(test);
        fieldTypeNestedArrayNullPacked test2 = serializer.deserialize(testBytes, fieldTypeNestedArrayNullPacked.class);
        assertNotNull(test2.field);
        assertEquals("do", test2.field.fields[0]);
        assertNull(test2.field.fields[1]);

        // Test start with null value
        test.field.fields = new String[] { null, "do" };
        testBytes = serializer.serialize(test);
        test2 = serializer.deserialize(testBytes, fieldTypeNestedArrayNullPacked.class);
        assertNotNull(test2.field);
        assertNull(test2.field.fields[0]);
        assertEquals("do", test2.field.fields[1]);
    }
}