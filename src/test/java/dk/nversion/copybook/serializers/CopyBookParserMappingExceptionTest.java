package dk.nversion.copybook.serializers;

import dk.nversion.copybook.exceptions.CopyBookException;
import dk.nversion.copybook.annotations.CopyBook;
import dk.nversion.copybook.annotations.CopyBookLine;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

public class CopyBookParserMappingExceptionTest {
    @Rule
    public ExpectedException expectedEx = ExpectedException.none();

    @CopyBook()
    public class IntegerToString {
        @CopyBookLine("01 FIELD PIC 9(8).")
        private String field;
    }

    @CopyBook()
    public class StringToInteger {
        @CopyBookLine("01 FIELD PIC X(8).")
        private int field;
    }

    @CopyBook()
    public class SignedDecimalToString {
        @CopyBookLine("01 FIELD PIC S9(16)V9(2).")
        private String field;
    }

    @CopyBook()
    public class ObjectToString {
        @CopyBookLine("01 OBJS.")
        private String fields;
    }

    @CopyBook()
    public class IntegerArrayToStringArray {
        @CopyBookLine("01 FIELDS OCCURS 10 TIMES.")
        @CopyBookLine("02 FIELD PIC 9(8).")
        private String[] fields;
    }

    @CopyBook()
    public class StringArrayToIntegerArray {
        @CopyBookLine("01 FIELDS OCCURS 10 TIMES.")
        @CopyBookLine("02 FIELD PIC X(8).")
        private int[] fields;
    }

    @CopyBook()
    public class ArrayOfObjectToStringArray {
        @CopyBookLine("01 OBJS OCCURS 10 TIMES.")
        private String[] fields;
    }

    @CopyBook()
    public class IntegerArrayToInteger {
        @CopyBookLine("01 FIELDS OCCURS 10 TIMES.")
        @CopyBookLine("02 FIELD PIC 9(8).")
        private int fields;
    }

    @CopyBook()
    public class StringArrayToString {
        @CopyBookLine("01 FIELDS OCCURS 10 TIMES.")
        @CopyBookLine("02 FIELD PIC X(8).")
        private String fields;
    }

    @CopyBook()
    public class ArrayOfObjectToString {
        @CopyBookLine("01 OBJS OCCURS 10 TIMES.")
        private String fields;
    }


    @Test
    public void testIntegerToString() throws Exception {
        expectedEx.expect(CopyBookException.class);
        expectedEx.expectMessage("could not find field converter defined for Integer to String");
        CopyBookParser copyBookParser = new CopyBookParser(IntegerToString.class);
    }

    @Test
    public void testStringToInteger() throws Exception {
        expectedEx.expect(CopyBookException.class);
        expectedEx.expectMessage("could not find field converter defined for String to Integer");
        CopyBookParser copyBookParser = new CopyBookParser(StringToInteger.class);
    }

    @Test
    public void testSignedDecimalToString() throws Exception {
        expectedEx.expect(CopyBookException.class);
        expectedEx.expectMessage("could not find field converter defined for SignedDecimal to String");
        CopyBookParser copyBookParser = new CopyBookParser(SignedDecimalToString.class);
    }

    @Test
    public void testObjectToString() throws Exception {
        expectedEx.expect(CopyBookException.class);
        expectedEx.expectMessage("not defining a type in the copybook line");
        CopyBookParser copyBookParser = new CopyBookParser(ObjectToString.class);
    }

    @Test
    public void testIntegerArrayToStringArray() throws Exception {
        expectedEx.expect(CopyBookException.class);
        expectedEx.expectMessage("could not find field converter defined for Integer to String");
        CopyBookParser copyBookParser = new CopyBookParser(IntegerArrayToStringArray.class);
    }

    @Test
    public void testStringArrayToIntegerArray() throws Exception {
        expectedEx.expect(CopyBookException.class);
        expectedEx.expectMessage("could not find field converter defined for String to Integer");
        CopyBookParser copyBookParser = new CopyBookParser(StringArrayToIntegerArray.class);
    }

    @Test
    public void testArrayOfObjectToStringArray() throws Exception {
        expectedEx.expect(CopyBookException.class);
        expectedEx.expectMessage("not defining a type in the copybook line");
        CopyBookParser copyBookParser = new CopyBookParser(ArrayOfObjectToStringArray.class);
    }

    @Test
    public void testStringArrayToString() throws Exception {
        expectedEx.expect(CopyBookException.class);
        expectedEx.expectMessage("map array type to non array type");
        CopyBookParser copyBookParser = new CopyBookParser(StringArrayToString.class);
    }

    @Test
    public void testIntegerArrayToInteger() throws Exception {
        expectedEx.expect(CopyBookException.class);
        expectedEx.expectMessage("map array type to non array type");
        CopyBookParser copyBookParser = new CopyBookParser(IntegerArrayToInteger.class);
    }

    @Test
    public void testArrayOfObjectToString() throws Exception {
        expectedEx.expect(CopyBookException.class);
        expectedEx.expectMessage("map array type to non array type");
        CopyBookParser copyBookParser = new CopyBookParser(ArrayOfObjectToString.class);
    }
}
