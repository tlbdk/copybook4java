package dk.nversion.copybook.serializers;

import dk.nversion.copybook.annotations.CopyBook;
import dk.nversion.copybook.annotations.CopyBookLine;

import java.math.BigDecimal;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

public class CopyBookParserFormatTest {

    // String / String Array
    @CopyBook()
    public class StringToString {
        @CopyBookLine("01 FIELD PIC X(8).")
        private String field;
    }
    @CopyBook()
    public class StringArrayToStringArray {
        @CopyBookLine("01 FIELDS OCCURS 10 TIMES PIC X(8).")
        private String[] fields;
    }
    @CopyBook()
    public class StringArrayToStringArrayMultiLine {
        @CopyBookLine("01 FIELDS OCCURS 10 TIMES.")
        @CopyBookLine("02 FIELD PIC X(8).")
        private String[] fields;
    }

    @org.junit.Test
    public void testStringToString() throws Exception {
        CopyBookParser copyBookParser = new CopyBookParser(StringToString.class);
        CopyBookField field = copyBookParser.getConfig().getFields().get(0);
        assertEquals(8, field.getSize());
    }
    @org.junit.Test
    public void testStringArrayToStringArray() throws Exception {
        CopyBookParser copyBookParser = new CopyBookParser(StringArrayToStringArray.class);
        CopyBookField field = copyBookParser.getConfig().getFields().get(0);
        assertTrue(field.isArray());
        assertEquals(8, field.getSize());
        assertEquals(10, field.getMaxOccurs());
        assertEquals(10, field.getMinOccurs());
    }
    @org.junit.Test
    public void testStringArrayToStringArrayMultiLine() throws Exception {
        CopyBookParser copyBookParser = new CopyBookParser(StringArrayToStringArrayMultiLine.class);
        CopyBookField field = copyBookParser.getConfig().getFields().get(0);
        assertTrue(field.isArray());
        assertEquals(8, field.getSize());
        assertEquals(10, field.getMaxOccurs());
        assertEquals(10, field.getMinOccurs());
    }


    // Integer / Integer Array
    @CopyBook()
    public class IntegerToInteger {
        @CopyBookLine("01 FIELD PIC 9(8).")
        private int field;
    }
    @CopyBook()
    public class IntegerArrayToIntegerArray {
        @CopyBookLine("01 FIELDS OCCURS 10 TIMES PIC 9(8).")
        private int[] fields;
    }
    @CopyBook()
    public class IntegerArrayToIntegerArrayMultiLine {
        @CopyBookLine("01 FIELDS OCCURS 10 TIMES.")
        @CopyBookLine("02 FIELD PIC 9(8).")
        private int[] fields;
    }

    @org.junit.Test
    public void testIntegerToInteger() throws Exception {
        CopyBookParser copyBookParser = new CopyBookParser(IntegerToInteger.class);
        CopyBookField field = copyBookParser.getConfig().getFields().get(0);
        assertEquals(8, field.getSize());
    }
    @org.junit.Test
    public void testIntegerArrayToIntegerArray() throws Exception {
        CopyBookParser copyBookParser = new CopyBookParser(IntegerArrayToIntegerArray.class);
        CopyBookField field = copyBookParser.getConfig().getFields().get(0);
        assertTrue(field.isArray());
        assertEquals(8, field.getSize());
        assertEquals(10, field.getMaxOccurs());
        assertEquals(10, field.getMinOccurs());
    }
    @org.junit.Test
    public void testIntegerArrayToIntegerArrayMultiLine() throws Exception {
        CopyBookParser copyBookParser = new CopyBookParser(IntegerArrayToIntegerArrayMultiLine.class);
        CopyBookField field = copyBookParser.getConfig().getFields().get(0);
        assertTrue(field.isArray());
        assertEquals(8, field.getSize());
        assertEquals(10, field.getMaxOccurs());
        assertEquals(10, field.getMinOccurs());
    }


    // Signed Integer / Signed Integer Array
    @CopyBook()
    public class SignedIntegerToInteger {
        @CopyBookLine("01 FIELD PIC S9(16).")
        private int field;
    }
    @CopyBook()
    public class SignedIntegerArrayToIntegerArray {
        @CopyBookLine("01 FIELDS OCCURS 10 TIMES PIC S9(8).")
        private int[] fields;
    }
    @CopyBook()
    public class SignedIntegerArrayToIntegerArrayMultiLine {
        @CopyBookLine("01 FIELDS OCCURS 10 TIMES.")
        @CopyBookLine("02 FIELD PIC S9(8).")
        private int[] fields;
    }

    @org.junit.Test
    public void testSignedIntegerToInteger() throws Exception {
        CopyBookParser copyBookParser = new CopyBookParser(SignedIntegerToInteger.class);
        CopyBookField field = copyBookParser.getConfig().getFields().get(0);
        assertEquals(16, field.getSize());
    }
    @org.junit.Test
    public void testSignedIntegerArrayToIntegerArray() throws Exception {
        CopyBookParser copyBookParser = new CopyBookParser(SignedIntegerArrayToIntegerArray.class);
        CopyBookField field = copyBookParser.getConfig().getFields().get(0);
        assertTrue(field.isArray());
        assertEquals(8, field.getSize());
        assertEquals(10, field.getMaxOccurs());
        assertEquals(10, field.getMinOccurs());
    }
    @org.junit.Test
    public void testSignedIntegerArrayToIntegerArrayMultiLine() throws Exception {
        CopyBookParser copyBookParser = new CopyBookParser(SignedIntegerArrayToIntegerArrayMultiLine.class);
        CopyBookField field = copyBookParser.getConfig().getFields().get(0);
        assertTrue(field.isArray());
        assertEquals(8, field.getSize());
        assertEquals(10, field.getMaxOccurs());
        assertEquals(10, field.getMinOccurs());
    }


    // Decimal / Decimal Array
    @CopyBook()
    public class DecimalToBigDecimal {
        @CopyBookLine("01 FIELD PIC 9(16)V9(2).")
        private BigDecimal field;
    }
    @CopyBook()
    public class DecimalArrayToDecimalArray {
        @CopyBookLine("01 FIELDS OCCURS 10 TIMES PIC 9(8)V9(2).")
        private BigDecimal[] fields;
    }
    @CopyBook()
    public class DecimalArrayToDecimalArrayMultiLine {
        @CopyBookLine("01 FIELDS OCCURS 10 TIMES.")
        @CopyBookLine("02 FIELD PIC 9(8)V9(2).")
        private BigDecimal[] fields;
    }

    @org.junit.Test
    public void testDecimalToBigDecimal() throws Exception {
        CopyBookParser copyBookParser = new CopyBookParser(DecimalToBigDecimal.class);
        CopyBookField field = copyBookParser.getConfig().getFields().get(0);
        assertEquals(18, field.getSize());
        assertEquals(2, field.getDecimals());
    }
    @org.junit.Test
    public void testDecimalArrayToBigDecimalArray() throws Exception {
        CopyBookParser copyBookParser = new CopyBookParser(DecimalArrayToDecimalArray.class);
        CopyBookField field = copyBookParser.getConfig().getFields().get(0);
        assertTrue(field.isArray());
        assertEquals(10, field.getSize());
        assertEquals(2, field.getDecimals());
        assertEquals(10, field.getMaxOccurs());
        assertEquals(10, field.getMinOccurs());
    }
    @org.junit.Test
    public void testDecimalArrayToDecimalArrayMultiLine() throws Exception {
        CopyBookParser copyBookParser = new CopyBookParser(DecimalArrayToDecimalArrayMultiLine.class);
        CopyBookField field = copyBookParser.getConfig().getFields().get(0);
        assertTrue(field.isArray());
        assertEquals(10, field.getSize());
        assertEquals(2, field.getDecimals());
        assertEquals(10, field.getMaxOccurs());
        assertEquals(10, field.getMinOccurs());
    }


    // Signed Decimal / Signed Decimal Array
    @CopyBook()
    public class SignedDecimalToBigDecimal {
        @CopyBookLine("01 FIELD PIC S9(16)V9(2).")
        private BigDecimal field;
    }
    @CopyBook()
    public class SignedDecimalArrayToDecimalArray {
        @CopyBookLine("01 FIELDS OCCURS 10 TIMES PIC S9(8)V9(2).")
        private BigDecimal[] fields;
    }
    @CopyBook()
    public class SignedDecimalArrayToDecimalArrayMultiLine {
        @CopyBookLine("01 FIELDS OCCURS 10 TIMES.")
        @CopyBookLine("02 FIELD PIC S9(8)V9(2).")
        private BigDecimal[] fields;
    }

    @org.junit.Test
    public void testSignedDecimalToBigDecimal() throws Exception {
        CopyBookParser copyBookParser = new CopyBookParser(SignedDecimalToBigDecimal.class);
        CopyBookField field = copyBookParser.getConfig().getFields().get(0);
        assertEquals(18, field.getSize());
        assertEquals(2, field.getDecimals());
    }
    @org.junit.Test
    public void testSignedDecimalArrayToDecimalArray() throws Exception {
        CopyBookParser copyBookParser = new CopyBookParser(SignedDecimalArrayToDecimalArray.class);
        CopyBookField field = copyBookParser.getConfig().getFields().get(0);
        assertTrue(field.isArray());
        assertEquals(10, field.getSize());
        assertEquals(2, field.getDecimals());
        assertEquals(10, field.getMaxOccurs());
        assertEquals(10, field.getMinOccurs());
    }
    @org.junit.Test
    public void testSignedDecimalArrayToDecimalArrayMultiLine() throws Exception {
        CopyBookParser copyBookParser = new CopyBookParser(SignedDecimalArrayToDecimalArrayMultiLine.class);
        CopyBookField field = copyBookParser.getConfig().getFields().get(0);
        assertTrue(field.isArray());
        assertEquals(10, field.getSize());
        assertEquals(2, field.getDecimals());
        assertEquals(10, field.getMaxOccurs());
        assertEquals(10, field.getMinOccurs());
    }


    // Object / Object Array
    @CopyBook()
    public class ObjectWithStringToString {
        @CopyBookLine("01 OBJS.")
        private StringToString fields;
    }
    @CopyBook()
    public class ObjectArrayWithStringToString {
        @CopyBookLine("01 OBJS OCCURS 10 TIMES.")
        private StringToString[] fields;
    }

    @org.junit.Test
    public void testObjectWithStringToString() throws Exception {
        CopyBookParser copyBookParser = new CopyBookParser(ObjectWithStringToString.class);
        CopyBookField field = copyBookParser.getConfig().getFields().get(0);
        assertEquals(8, field.getSubCopyBookFields().get(0).getSize());
    }

    @org.junit.Test
    public void testObjectArrayWithStringToString() throws Exception {
        CopyBookParser copyBookParser = new CopyBookParser(ObjectArrayWithStringToString.class);
        CopyBookField field = copyBookParser.getConfig().getFields().get(0);
        assertEquals(8, field.getSubCopyBookFields().get(0).getSize());
        assertEquals(10, field.getMaxOccurs());
        assertEquals(10, field.getMinOccurs());
    }

    // Depending on fields
    @CopyBook()
    public class StringArrayToStringArrayDependingOn {
        @CopyBookLine("02 COUNT PIC 9(2).")
        private int count;
        @CopyBookLine("01 FIELDS OCCURS 0 TO 10 TIMES PIC X(8) DEPENDING ON COUNT.")
        private String[] fields;
    }
    @CopyBook()
    public class StringArrayToStringArrayDependingOnMultiLine {
        @CopyBookLine("02 COUNT PIC 9(2).")
        private int count;
        @CopyBookLine("01 FIELDS OCCURS 0 TO 10 TIMES DEPENDING ON COUNT.")
        @CopyBookLine("02 FIELD PIC X(8).")
        private String[] fields;
    }

    @org.junit.Test
    public void testStringArrayToStringArrayDependingOn() throws Exception {
        CopyBookParser copyBookParser = new CopyBookParser(StringArrayToStringArrayDependingOn.class);
        CopyBookField field = copyBookParser.getConfig().getFields().get(1);
        assertTrue(field.isArray());
        assertEquals(8, field.getSize());
        assertEquals(0, field.getMinOccurs());
        assertEquals(10, field.getMaxOccurs());
    }
    @org.junit.Test
    public void testStringArrayToStringArrayDependingOnMultiLine() throws Exception {
        CopyBookParser copyBookParser = new CopyBookParser(StringArrayToStringArrayDependingOnMultiLine.class);
        CopyBookField field = copyBookParser.getConfig().getFields().get(1);
        assertTrue(field.isArray());
        assertEquals(8, field.getSize());
        assertEquals(0, field.getMinOccurs());
        assertEquals(10, field.getMaxOccurs());
    }

    // Depending on fields in subfield
    @CopyBook()
    public class CounterDependingInOn {
        @CopyBookLine("02 COUNT PIC 9(2).")
        private int count;
    }
    @CopyBook()
    public class StringArrayToStringArrayDependingInOn {
        @CopyBookLine("01 SUBFIELD.")
        private CounterDependingInOn subfield;
        @CopyBookLine("01 RESULT.")
        @CopyBookLine("02 FIELDS OCCURS 0 TO 10 TIMES PIC X(8) DEPENDING ON COUNT IN SUBFIELD.")
        private String[] fields;
    }
    @CopyBook()
    public class StringArrayToStringArrayDependingOnInMultiLine {
        @CopyBookLine("01 SUBFIELD.")
        private CounterDependingInOn subfield;
        @CopyBookLine("01 RESULT.")
        @CopyBookLine("02 FIELDS OCCURS 0 TO 10 TIMES DEPENDING ON COUNT IN SUBFIELD.")
        @CopyBookLine("03 FIELD PIC X(8).")
        private String[] fields;
    }

    @org.junit.Test
    public void testStringArrayToStringArrayDependingOnIn() throws Exception {
        CopyBookParser copyBookParser = new CopyBookParser(StringArrayToStringArrayDependingInOn.class);
        CopyBookField field = copyBookParser.getConfig().getFields().get(1);
        assertTrue(field.isArray());
        assertEquals(8, field.getSize());
        assertEquals(0, field.getMinOccurs());
        assertEquals(10, field.getMaxOccurs());
    }
    @org.junit.Test
    public void testStringArrayToStringArrayDependingOnInMultiLine() throws Exception {
        CopyBookParser copyBookParser = new CopyBookParser(StringArrayToStringArrayDependingOnInMultiLine.class);
        CopyBookField field = copyBookParser.getConfig().getFields().get(1);
        assertTrue(field.isArray());
        assertEquals(8, field.getSize());
        assertEquals(0, field.getMinOccurs());
        assertEquals(10, field.getMaxOccurs());
    }
}