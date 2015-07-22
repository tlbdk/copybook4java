package dk.nversion.copybook;

import dk.nversion.copybook.packed.RequestMessage;
import dk.nversion.copybook.packed.RequestTest;
import org.junit.Rule;
import org.junit.rules.ExpectedException;

import static org.junit.Assert.assertEquals;

public class SerializerFieldValidationTestExceptionsTest {

    @Rule
    public ExpectedException expectedEx = ExpectedException.none();

    @CopyBook()
    public class WrongFieldTypeInteger {
        @CopyBookLine("01 FIELD PIC 9(8).")
        private String field;
    }

    @CopyBook()
    public class WrongFieldTypeString {
        @CopyBookLine("01 FIELD PIC X(8).")
        private int field;
    }

    @CopyBook()
    public class WrongFieldTypeDecimal {
        @CopyBookLine("01 FIELD PIC 9(8)V9(3).")
        private String field;
    }

    @CopyBook()
    public class WrongFieldTypeObject {
        @CopyBookLine("01 FIELD.")
        private String field;
    }

    @CopyBook()
    public class WrongFieldTypeIntArray {
        @CopyBookLine("01 FIELDS OCCURS 10 TIMES.")
        @CopyBookLine("02 FIELD PIC 9(8).")
        private String field;
    }

    @CopyBook()
    public class WrongFieldTypeStringArray {
        @CopyBookLine("01 FIELDS OCCURS 10 TIMES.")
        @CopyBookLine("02 FIELD PIC X(8).")
        private String field;
    }

    @CopyBook()
    public class WrongFieldTypeObjectArray {
        @CopyBookLine("01 OBJS OCCURS 10 TIMES.")
        private String field;
    }

    @org.junit.Test
    public void testWrongFieldTypeInteger() throws Exception {
        expectedEx.expect(CopyBookException.class);
        expectedEx.expectMessage("not one of the supported types(int, long, BigInteger)");
        CopyBookSerializer requestTestSerializer = new CopyBookSerializer(WrongFieldTypeInteger.class);
    }

    @org.junit.Test
    public void testWrongFieldTypeString() throws Exception {
        expectedEx.expect(CopyBookException.class);
        expectedEx.expectMessage("not one of the supported types(String)");
        CopyBookSerializer requestTestSerializer = new CopyBookSerializer(WrongFieldTypeString.class);
    }

    @org.junit.Test
    public void testWrongFieldTypeDecimal() throws Exception {
        expectedEx.expect(CopyBookException.class);
        expectedEx.expectMessage("not one of the supported types(float, double, BigDecimal)");
        CopyBookSerializer requestTestSerializer = new CopyBookSerializer(WrongFieldTypeDecimal.class);
    }

    @org.junit.Test
    public void testWrongFieldTypeObject() throws Exception {
        expectedEx.expect(CopyBookException.class);
        expectedEx.expectMessage("Could not parse the PIC type");
        CopyBookSerializer requestTestSerializer = new CopyBookSerializer(WrongFieldTypeObject.class);
    }

    @org.junit.Test
    public void testWrongFieldTypeIntArray() throws Exception {
        expectedEx.expect(CopyBookException.class);
        expectedEx.expectMessage("is not an array type");
        CopyBookSerializer requestTestSerializer = new CopyBookSerializer(WrongFieldTypeIntArray.class);
    }

    @org.junit.Test
    public void testWrongFieldTypeStringArray() throws Exception {
        expectedEx.expect(CopyBookException.class);
        expectedEx.expectMessage("is not an array type");
        CopyBookSerializer requestTestSerializer = new CopyBookSerializer(WrongFieldTypeStringArray.class);
    }

    @org.junit.Test
    public void testWrongFieldTypeObjectArray() throws Exception {
        expectedEx.expect(CopyBookException.class);
        expectedEx.expectMessage("should be an array type");
        CopyBookSerializer requestTestSerializer = new CopyBookSerializer(WrongFieldTypeObjectArray.class);
    }
}