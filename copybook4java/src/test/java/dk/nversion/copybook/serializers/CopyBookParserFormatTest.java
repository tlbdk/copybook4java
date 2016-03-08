/*
 * Copyright (c) 2015. Troels Liebe Bentsen <tlb@nversion.dk>
 * Licensed under the MIT license (LICENSE.txt)
 */

package dk.nversion.copybook.serializers;

import dk.nversion.copybook.annotations.CopyBook;
import dk.nversion.copybook.annotations.CopyBookLine;
import dk.nversion.copybook.annotations.CopyBookRedefine;
import org.junit.Test;

import java.math.BigDecimal;
import java.util.List;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

public class CopyBookParserFormatTest {

    @CopyBook()
    public class FormatTestString {
        @CopyBookLine("04 FIELD1 PIC X.")
        private String string1;
        @CopyBookLine("04 FIELD1 PIC XX.")
        private String string2;
        @CopyBookLine("04 FIELD1 PIC X(1).")
        private String string3;
        @CopyBookLine("04 FIELD1 PIC X(2).")
        private String string4;
        @CopyBookLine("04 FIELD1 PIC X(10).")
        private String string5;
    }

    @Test
    public void testFormatTestString() throws Exception {
        CopyBookParser copyBookParser = new CopyBookParser(FormatTestString.class);
        List<CopyBookField> fields = copyBookParser.getConfig().getFields();
        assertEquals(1, fields.get(0).getSize());
        assertEquals(fields.get(0).getConverter().getClass().getSimpleName(), "StringToString");
        assertEquals(2, fields.get(1).getSize());
        assertEquals(fields.get(1).getConverter().getClass().getSimpleName(), "StringToString");
        assertEquals(1, fields.get(2).getSize());
        assertEquals(fields.get(2).getConverter().getClass().getSimpleName(), "StringToString");
        assertEquals(2, fields.get(3).getSize());
        assertEquals(fields.get(3).getConverter().getClass().getSimpleName(), "StringToString");
        assertEquals(10, fields.get(4).getSize());
        assertEquals(fields.get(4).getConverter().getClass().getSimpleName(), "StringToString");
    }

    @CopyBook()
    public class FormatTestInteger {
        @CopyBookLine("04 FIELD1 PIC 9.")
        private int integer1;
        @CopyBookLine("04 FIELD1 PIC 99.")
        private int integer2;
        @CopyBookLine("04 FIELD1 PIC 9(1).")
        private int integer3;
        @CopyBookLine("04 FIELD1 PIC 9(2).")
        private int integer4;
        @CopyBookLine("04 FIELD1 PIC 9(10).")
        private long integer5;
    }

    @Test
    public void testFormatTestInteger() throws Exception {
        CopyBookParser copyBookParser = new CopyBookParser(FormatTestInteger.class);
        List<CopyBookField> fields = copyBookParser.getConfig().getFields();
        assertEquals(1, fields.get(0).getSize());
        assertEquals(fields.get(0).getConverter().getClass().getSimpleName(), "IntegerToInteger");
        assertEquals(2, fields.get(1).getSize());
        assertEquals(fields.get(1).getConverter().getClass().getSimpleName(), "IntegerToInteger");
        assertEquals(1, fields.get(2).getSize());
        assertEquals(fields.get(2).getConverter().getClass().getSimpleName(), "IntegerToInteger");
        assertEquals(2, fields.get(3).getSize());
        assertEquals(fields.get(3).getConverter().getClass().getSimpleName(), "IntegerToInteger");
        assertEquals(10, fields.get(4).getSize());
        assertEquals(fields.get(4).getConverter().getClass().getSimpleName(), "IntegerToLong");
    }

    @CopyBook()
    public class FormatTestSignedInteger {
        @CopyBookLine("04 FIELD1 PIC S9.")
        private int signedinteger1;
        @CopyBookLine("04 FIELD1 PIC S99.")
        private int signedinteger2;
        @CopyBookLine("04 FIELD1 PIC S9(1).")
        private int signedinteger3;
        @CopyBookLine("04 FIELD1 PIC S9(2).")
        private int signedinteger4;
        @CopyBookLine("04 FIELD1 PIC S9(10).")
        private int signedinteger5;
    }

    @Test
    public void testFormatTestSignedInteger() throws Exception {
        CopyBookParser copyBookParser = new CopyBookParser(FormatTestSignedInteger.class);
        List<CopyBookField> fields = copyBookParser.getConfig().getFields();
        assertEquals(1, fields.get(0).getSize());
        assertEquals(fields.get(0).getConverter().getClass().getSimpleName(), "SignedIntegerToInteger");
        assertEquals(2, fields.get(1).getSize());
        assertEquals(fields.get(1).getConverter().getClass().getSimpleName(), "SignedIntegerToInteger");
        assertEquals(1, fields.get(2).getSize());
        assertEquals(fields.get(2).getConverter().getClass().getSimpleName(), "SignedIntegerToInteger");
        assertEquals(2, fields.get(3).getSize());
        assertEquals(fields.get(3).getConverter().getClass().getSimpleName(), "SignedIntegerToInteger");
        assertEquals(10, fields.get(4).getSize());
        assertEquals(fields.get(4).getConverter().getClass().getSimpleName(), "SignedIntegerToInteger");
    }


    @CopyBook()
    public class FormatTestDecimal {
        @CopyBookLine("04 FIELD1 PIC 9V9.")
        private BigDecimal decimal1;
        @CopyBookLine("04 FIELD1 PIC 99V99.")
        private BigDecimal decimal2;
        @CopyBookLine("04 FIELD1 PIC 9(1)V9.")
        private BigDecimal decimal3;
        @CopyBookLine("04 FIELD1 PIC 9(1)V99.")
        private BigDecimal decimal4;
        @CopyBookLine("04 FIELD1 PIC 9(1)V9(1).")
        private BigDecimal decimal5;
        @CopyBookLine("04 FIELD1 PIC 9(2)V9(2).")
        private BigDecimal decimal6;
        @CopyBookLine("04 FIELD1 PIC 9(10)V9(10).")
        private BigDecimal decimal7;
    }

    @Test
    public void testFormatTestDecimal() throws Exception {
        CopyBookParser copyBookParser = new CopyBookParser(FormatTestDecimal.class);
        List<CopyBookField> fields = copyBookParser.getConfig().getFields();
        assertEquals(2, fields.get(0).getSize());
        assertEquals(1, fields.get(0).getDecimals());
        assertEquals(fields.get(0).getConverter().getClass().getSimpleName(), "DecimalToBigDecimal");
        assertEquals(4, fields.get(1).getSize());
        assertEquals(2, fields.get(1).getDecimals());
        assertEquals(fields.get(1).getConverter().getClass().getSimpleName(), "DecimalToBigDecimal");
        assertEquals(2, fields.get(2).getSize());
        assertEquals(1, fields.get(2).getDecimals());
        assertEquals(fields.get(2).getConverter().getClass().getSimpleName(), "DecimalToBigDecimal");
        assertEquals(3, fields.get(3).getSize());
        assertEquals(2, fields.get(3).getDecimals());
        assertEquals(fields.get(3).getConverter().getClass().getSimpleName(), "DecimalToBigDecimal");
        assertEquals(2, fields.get(4).getSize());
        assertEquals(1, fields.get(4).getDecimals());
        assertEquals(fields.get(4).getConverter().getClass().getSimpleName(), "DecimalToBigDecimal");
        assertEquals(4, fields.get(5).getSize());
        assertEquals(2, fields.get(5).getDecimals());
        assertEquals(fields.get(5).getConverter().getClass().getSimpleName(), "DecimalToBigDecimal");
        assertEquals(20, fields.get(6).getSize());
        assertEquals(10, fields.get(6).getDecimals());
        assertEquals(fields.get(6).getConverter().getClass().getSimpleName(), "DecimalToBigDecimal");
    }

    @CopyBook()
    public class FormatTestSignedDecimal {
        @CopyBookLine("04 FIELD1 PIC S9V9.")
        private BigDecimal signeddecimal1;
        @CopyBookLine("04 FIELD1 PIC S99V99.")
        private BigDecimal signeddecimal2;
        @CopyBookLine("04 FIELD1 PIC S9(1)V9.")
        private BigDecimal signeddecimal3;
        @CopyBookLine("04 FIELD1 PIC S9(1)V99.")
        private BigDecimal signeddecimal4;
        @CopyBookLine("04 FIELD1 PIC S9(1)V9(1).")
        private BigDecimal signeddecimal5;
        @CopyBookLine("04 FIELD1 PIC S9(2)V9(2).")
        private BigDecimal signeddecimal6;
        @CopyBookLine("04 FIELD1 PIC S9(10)V9(10).")
        private BigDecimal signeddecimal7;
    }

    @Test
    public void testFormatTestSignedDecimal() throws Exception {
        CopyBookParser copyBookParser = new CopyBookParser(FormatTestSignedDecimal.class);
        List<CopyBookField> fields = copyBookParser.getConfig().getFields();
        assertEquals(2, fields.get(0).getSize());
        assertEquals(1, fields.get(0).getDecimals());
        assertEquals(fields.get(0).getConverter().getClass().getSimpleName(), "SignedDecimalToBigDecimal");
        assertEquals(4, fields.get(1).getSize());
        assertEquals(2, fields.get(1).getDecimals());
        assertEquals(fields.get(1).getConverter().getClass().getSimpleName(), "SignedDecimalToBigDecimal");
        assertEquals(2, fields.get(2).getSize());
        assertEquals(1, fields.get(2).getDecimals());
        assertEquals(fields.get(2).getConverter().getClass().getSimpleName(), "SignedDecimalToBigDecimal");
        assertEquals(3, fields.get(3).getSize());
        assertEquals(2, fields.get(3).getDecimals());
        assertEquals(fields.get(3).getConverter().getClass().getSimpleName(), "SignedDecimalToBigDecimal");
        assertEquals(2, fields.get(4).getSize());
        assertEquals(1, fields.get(4).getDecimals());
        assertEquals(fields.get(4).getConverter().getClass().getSimpleName(), "SignedDecimalToBigDecimal");
        assertEquals(4, fields.get(5).getSize());
        assertEquals(2, fields.get(5).getDecimals());
        assertEquals(fields.get(5).getConverter().getClass().getSimpleName(), "SignedDecimalToBigDecimal");
        assertEquals(20, fields.get(6).getSize());
        assertEquals(10, fields.get(6).getDecimals());
        assertEquals(fields.get(6).getConverter().getClass().getSimpleName(), "SignedDecimalToBigDecimal");
    }

    // Format test
    @CopyBook()
    public class FormatTestComplex {
        @CopyBookLine("04 FIELD1 PIC 9.")
        private Integer integer1;

        @CopyBookLine("04 FIELD1 PIC S9.")
        private Integer signedinteger1;

        @CopyBookLine("04 FIELD1 PIC 9.")
        private Long long1;

        @CopyBookLine("04 FIELD1 PIC S9.")
        private Long signedlong1;
    }

    @org.junit.Test()
    public void testFormatTestComplex() throws Exception {
        CopyBookParser copyBookParser = new CopyBookParser(FormatTestComplex.class);
        List<CopyBookField> fields = copyBookParser.getConfig().getFields();
    }

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
        private long field;
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

    @org.junit.Test
    public void testDecimalToBigDecimal() throws Exception {
        CopyBookParser copyBookParser = new CopyBookParser(DecimalToBigDecimal.class);
        CopyBookField field = copyBookParser.getConfig().getFields().get(0);
        assertEquals(18, field.getSize());
        assertEquals(2, field.getDecimals());
    }

    @CopyBook()
    public class DecimalArrayToDecimalArray {
        @CopyBookLine("01 FIELDS OCCURS 10 TIMES PIC 9(8)V9(2).")
        private BigDecimal[] fields;
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

    @CopyBook()
    public class DecimalArrayToDecimalArrayMultiLine {
        @CopyBookLine("01 FIELDS OCCURS 10 TIMES.")
        @CopyBookLine("02 FIELD PIC 9(8)V9(2).")
        private BigDecimal[] fields;
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

    @CopyBook()
    public class DecimalArrayToDecimalArrayMultiLineWithOutTimes {
        @CopyBookLine("01 FIELDS OCCURS 10.")
        @CopyBookLine("02 FIELD PIC 9(8)V9(2).")
        private BigDecimal[] fields;
    }

    @org.junit.Test
    public void testDecimalArrayToDecimalArrayMultiLineWithOutTimes() throws Exception {
        CopyBookParser copyBookParser = new CopyBookParser(DecimalArrayToDecimalArrayMultiLineWithOutTimes.class);
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

    @org.junit.Test
    public void testStringArrayToStringArrayDependingOn() throws Exception {
        CopyBookParser copyBookParser = new CopyBookParser(StringArrayToStringArrayDependingOn.class);
        CopyBookField field = copyBookParser.getConfig().getFields().get(1);
        assertTrue(field.isArray());
        assertEquals(8, field.getSize());
        assertEquals(0, field.getMinOccurs());
        assertEquals(10, field.getMaxOccurs());
        assertEquals(field.getCounterKey(), "COUNT");
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
    public void testStringArrayToStringArrayDependingOnMultiLine() throws Exception {
        CopyBookParser copyBookParser = new CopyBookParser(StringArrayToStringArrayDependingOnMultiLine.class);
        CopyBookField field = copyBookParser.getConfig().getFields().get(1);
        assertTrue(field.isArray());
        assertEquals(8, field.getSize());
        assertEquals(0, field.getMinOccurs());
        assertEquals(10, field.getMaxOccurs());
        assertEquals(field.getCounterKey(), "COUNT");
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

    @org.junit.Test
    public void testStringArrayToStringArrayDependingOnIn() throws Exception {
        CopyBookParser copyBookParser = new CopyBookParser(StringArrayToStringArrayDependingInOn.class);
        CopyBookField field = copyBookParser.getConfig().getFields().get(1);
        assertTrue(field.isArray());
        assertEquals(8, field.getSize());
        assertEquals(0, field.getMinOccurs());
        assertEquals(10, field.getMaxOccurs());
        assertEquals(field.getCounterKey(), "SUBFIELD.COUNT");
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
    public void testStringArrayToStringArrayDependingOnInMultiLine() throws Exception {
        CopyBookParser copyBookParser = new CopyBookParser(StringArrayToStringArrayDependingOnInMultiLine.class);
        CopyBookField field = copyBookParser.getConfig().getFields().get(1);
        assertTrue(field.isArray());
        assertEquals(8, field.getSize());
        assertEquals(0, field.getMinOccurs());
        assertEquals(10, field.getMaxOccurs());
        assertEquals(field.getCounterKey(), "SUBFIELD.COUNT");
    }

    @CopyBook()
    public abstract class RedefinedField {
        @CopyBookLine("04 FIELD_ON PIC 9.")
        public int field_on;

        @CopyBookRedefine(on = "FIELD_ON", match = "1")
        @CopyBookLine("04 FIELD PIC XX.")
        public String field;

        @CopyBookRedefine(on = "FIELD_ON", match = "2")
        @CopyBookLine("04 FIELD1 REDEFINES FIELD PIC 99.")
        public int field1;

        @CopyBookRedefine(on = "FIELD_ON", match = "3")
        @CopyBookLine("04 FIELD2 REDEFINES FIELD PIC 9V9.")
        public BigDecimal field2;
    }

    @Test
    public void redefinedFieldTest() throws Exception {
        CopyBookParser copyBookParser = new CopyBookParser(RedefinedField.class);
        // TODO: Implement parsing of redefines
    }
    @CopyBook()
    public class NestedRedefinedSubFieldMain {
        @CopyBookLine("04 REDEFINED_FIELD.")
        RedefinedField redefinedField;
    }

    @CopyBook()
    public class RedefinedSubFieldMain {
        @CopyBookLine("04 FIELD_ON PIC 9.")
        public int field_on;

        @CopyBookRedefine(on = "FIELD_ON", match = "1")
        @CopyBookLine("04 FIELD.")
        public RedefinedSubFieldString field;

        @CopyBookRedefine(on = "FIELD_ON", match = "2")
        @CopyBookLine("04 FIELD1 REDEFINES FIELD.")
        public RedefinedSubFieldInt field1;

        @CopyBookRedefine(on = "FIELD_ON", match = "3")
        @CopyBookLine("04 FIELD2 REDEFINES FIELD.")
        public RedefinedSubFieldBigDecimal field2;
    }

    @CopyBook()
    public class RedefinedSubFieldString {
        @CopyBookLine("04 VALUE PIC X.")
        public String start;
        @CopyBookLine("04 VALUE PIC XX.")
        public String value;
    }

    @CopyBook()
    public class RedefinedSubFieldInt {
        @CopyBookLine("04 VALUE PIC X.")
        public String start;
        @CopyBookLine("04 VALUE PIC 99.")
        public int value;
    }

    @CopyBook()
    public class RedefinedSubFieldBigDecimal {
        @CopyBookLine("04 VALUE PIC X.")
        public String start;
        @CopyBookLine("04 VALUE PIC 9V9.")
        public BigDecimal value;
    }

    @Test
    public void redefinedSubFieldTest() throws Exception {
        CopyBookParser copyBookParser = new CopyBookParser(RedefinedSubFieldMain.class);

        // TODO: Implement test
        /* CopyBookField field = copyBookParser.getConfig().getFields().get(1);
        assertTrue(field.isArray());
        assertEquals(8, field.getSize());
        assertEquals(0, field.getMinOccurs());
        assertEquals(10, field.getMaxOccurs());
        assertEquals(field.getCounterKey(), "SUBFIELD.COUNT");
        */
    }

    @Test
    public void nestedRedefinedSubFieldTest() throws Exception {
        // TODO: Implement test
        CopyBookParser copyBookParser = new CopyBookParser(NestedRedefinedSubFieldMain.class);
        //System.out.print("");
    }

    // TODO - Implement date format: 05 EFFECTDATE  PIC X(8) DATE FORMAT YYYYXXXX.


}