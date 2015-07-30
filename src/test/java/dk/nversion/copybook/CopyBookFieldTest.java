package dk.nversion.copybook;

import org.junit.Before;
import org.junit.BeforeClass;

import java.lang.reflect.Field;
import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Map;

import static org.junit.Assert.*;

@CopyBookFieldFormat(fieldType = CopyBookFieldType.INT, rightPadding = false, paddingChar = '0', nullFillerChar = (byte)0, signingType = CopyBookFieldSigningType.PREFIX)
@CopyBookFieldFormat(fieldType = CopyBookFieldType.SIGNED_INT, rightPadding = false, paddingChar = '0', nullFillerChar = (byte)0, signingType = CopyBookFieldSigningType.PREFIX)
@CopyBookFieldFormat(fieldType = CopyBookFieldType.DECIMAL, rightPadding = false, paddingChar = '0', nullFillerChar = (byte)0, signingType = CopyBookFieldSigningType.PREFIX)
@CopyBookFieldFormat(fieldType = CopyBookFieldType.SIGNED_DECIMAL, rightPadding = false, paddingChar = '0', nullFillerChar = (byte)0, signingType = CopyBookFieldSigningType.PREFIX)
@CopyBookFieldFormat(fieldType = CopyBookFieldType.STRING, rightPadding = true, paddingChar  = ' ', nullFillerChar = (byte)0, signingType = CopyBookFieldSigningType.PREFIX)
public class CopyBookFieldTest {
    private String stringField;
    private int intField;
    private BigDecimal bigDecimalField;


    private Field[] stringFields;
    private Field[] intFields;
    private Field[] bigDecimalFields;

    private CopyBookField[] counters = new CopyBookField[] { null };
    private int[] indexes = new int[] { -1 };
    private int[] occurs = new int[] { 1 };
    private Map<CopyBookFieldType,CopyBookFieldFormat> paddingDefaults = new HashMap<CopyBookFieldType,CopyBookFieldFormat>();

    @Before
    public void setUp () throws NoSuchFieldException {
        stringFields = new Field[] { this.getClass().getDeclaredField("stringField") };
        intFields = new Field[] { this.getClass().getDeclaredField("intField") };
        bigDecimalFields = new Field[] { this.getClass().getDeclaredField("bigDecimalField") };
        CopyBookFieldFormat[] annotations = CopyBookFieldTest.class.getAnnotationsByType(CopyBookFieldFormat.class);
        for(CopyBookFieldFormat annotation : annotations) {
            paddingDefaults.put(annotation.fieldType(), annotation);
        }
    }

    // String

    @org.junit.Test
    public void testStringTypeX() throws Exception {
        CopyBookField field = new CopyBookField("04 FIELD PIC X.", StandardCharsets.UTF_8, stringFields, counters, indexes, occurs, paddingDefaults);
        assertEquals(1, field.size);
        assertEquals(CopyBookFieldType.STRING, field.type);
    }

    @org.junit.Test
    public void testStringTypeXX() throws Exception {
        CopyBookField field = new CopyBookField("04 FIELD PIC XX.", StandardCharsets.UTF_8, stringFields, counters, indexes, occurs, paddingDefaults);
        assertEquals(2, field.size);
        assertEquals(CopyBookFieldType.STRING, field.type);
    }

    @org.junit.Test
    public void testStringTypeX1() throws Exception {
        CopyBookField field = new CopyBookField("04 FIELD PIC X(1).", StandardCharsets.UTF_8, stringFields, counters, indexes, occurs, paddingDefaults);
        assertEquals(1, field.size);
        assertEquals(CopyBookFieldType.STRING, field.type);
    }

    @org.junit.Test
    public void testStringTypeX2() throws Exception {
        CopyBookField field = new CopyBookField("04 FIELD PIC X(2).", StandardCharsets.UTF_8, stringFields, counters, indexes, occurs, paddingDefaults);
        assertEquals(2, field.size);
        assertEquals(CopyBookFieldType.STRING, field.type);
    }

    @org.junit.Test
    public void testStringTypeX10() throws Exception {
        CopyBookField field = new CopyBookField("04 FIELD PIC X(10).", StandardCharsets.UTF_8, stringFields, counters, indexes, occurs, paddingDefaults);
        assertEquals(10, field.size);
        assertEquals(CopyBookFieldType.STRING, field.type);
    }

    // Unsigned integer

    @org.junit.Test
    public void testIntType9() throws Exception {
        CopyBookField field = new CopyBookField("04 FIELD PIC 9.", StandardCharsets.UTF_8, intFields, counters, indexes, occurs, paddingDefaults);
        assertEquals(1, field.size);
        assertEquals(CopyBookFieldType.INT, field.type);
    }

    @org.junit.Test
    public void testIntType99() throws Exception {
        CopyBookField field = new CopyBookField("04 FIELD PIC 99.", StandardCharsets.UTF_8, intFields, counters, indexes, occurs, paddingDefaults);
        assertEquals(2, field.size);
        assertEquals(CopyBookFieldType.INT, field.type);
    }

    @org.junit.Test
    public void testIntType91() throws Exception {
        CopyBookField field = new CopyBookField("04 FIELD PIC 9(1).", StandardCharsets.UTF_8, intFields, counters, indexes, occurs, paddingDefaults);
        assertEquals(1, field.size);
        assertEquals(CopyBookFieldType.INT, field.type);
    }

    @org.junit.Test
    public void testIntType92() throws Exception {
        CopyBookField field = new CopyBookField("04 FIELD PIC 9(2).", StandardCharsets.UTF_8, intFields, counters, indexes, occurs, paddingDefaults);
        assertEquals(2, field.size);
        assertEquals(CopyBookFieldType.INT, field.type);
    }

    @org.junit.Test
    public void testIntType910() throws Exception {
        CopyBookField field = new CopyBookField("04 FIELD PIC 9(10).", StandardCharsets.UTF_8, intFields, counters, indexes, occurs, paddingDefaults);
        assertEquals(10, field.size);
        assertEquals(CopyBookFieldType.INT, field.type);
    }

    // Signed integer

    @org.junit.Test
    public void testIntTypeS9() throws Exception {
        CopyBookField field = new CopyBookField("04 FIELD PIC S9.", StandardCharsets.UTF_8, intFields, counters, indexes, occurs, paddingDefaults);
        assertEquals(1, field.size);
        assertEquals(CopyBookFieldType.SIGNED_INT, field.type);
    }

    @org.junit.Test
    public void testIntTypeS99() throws Exception {
        CopyBookField field = new CopyBookField("04 FIELD PIC S99.", StandardCharsets.UTF_8, intFields, counters, indexes, occurs, paddingDefaults);
        assertEquals(2, field.size);
        assertEquals(CopyBookFieldType.SIGNED_INT, field.type);
    }

    @org.junit.Test
    public void testIntTypeS91() throws Exception {
        CopyBookField field = new CopyBookField("04 FIELD PIC S9(1).", StandardCharsets.UTF_8, intFields, counters, indexes, occurs, paddingDefaults);
        assertEquals(1, field.size);
        assertEquals(CopyBookFieldType.SIGNED_INT, field.type);
    }

    @org.junit.Test
    public void testIntTypeS92() throws Exception {
        CopyBookField field = new CopyBookField("04 FIELD PIC S9(2).", StandardCharsets.UTF_8, intFields, counters, indexes, occurs, paddingDefaults);
        assertEquals(2, field.size);
        assertEquals(CopyBookFieldType.SIGNED_INT, field.type);
    }

    @org.junit.Test
    public void testIntTypeS910() throws Exception {
        CopyBookField field = new CopyBookField("04 FIELD PIC S9(10).", StandardCharsets.UTF_8, intFields, counters, indexes, occurs, paddingDefaults);
        assertEquals(10, field.size);
        assertEquals(CopyBookFieldType.SIGNED_INT, field.type);
    }

    // Unsigned decimal

    @org.junit.Test
    public void testIntType9V99() throws Exception {
        CopyBookField field = new CopyBookField("04 FIELD PIC 9V9.", StandardCharsets.UTF_8, bigDecimalFields, counters, indexes, occurs, paddingDefaults);
        assertEquals(2, field.size);
        assertEquals(CopyBookFieldType.DECIMAL, field.type);
    }

    @org.junit.Test
    public void testIntType99V99() throws Exception {
        CopyBookField field = new CopyBookField("04 FIELD PIC 99V99.", StandardCharsets.UTF_8, bigDecimalFields, counters, indexes, occurs, paddingDefaults);
        assertEquals(4, field.size);
        assertEquals(CopyBookFieldType.DECIMAL, field.type);
    }

    @org.junit.Test
    public void testIntType91V9() throws Exception {
        CopyBookField field = new CopyBookField("04 FIELD PIC 9(1)V9.", StandardCharsets.UTF_8, bigDecimalFields, counters, indexes, occurs, paddingDefaults);
        assertEquals(2, field.size);
        assertEquals(CopyBookFieldType.DECIMAL, field.type);
    }

    @org.junit.Test
    public void testIntType91V99() throws Exception {
        CopyBookField field = new CopyBookField("04 FIELD PIC 9(1)V99.", StandardCharsets.UTF_8, bigDecimalFields, counters, indexes, occurs, paddingDefaults);
        assertEquals(3, field.size);
        assertEquals(CopyBookFieldType.DECIMAL, field.type);
    }

    @org.junit.Test
    public void testIntType91V91() throws Exception {
        CopyBookField field = new CopyBookField("04 FIELD PIC 9(1)V9(1).", StandardCharsets.UTF_8, bigDecimalFields, counters, indexes, occurs, paddingDefaults);
        assertEquals(2, field.size);
        assertEquals(CopyBookFieldType.DECIMAL, field.type);
    }

    @org.junit.Test
    public void testIntType92V92() throws Exception {
        CopyBookField field = new CopyBookField("04 FIELD PIC 9(2)V9(2).", StandardCharsets.UTF_8, bigDecimalFields, counters, indexes, occurs, paddingDefaults);
        assertEquals(4, field.size);
        assertEquals(CopyBookFieldType.DECIMAL, field.type);
    }

    @org.junit.Test
    public void testIntType910V910() throws Exception {
        CopyBookField field = new CopyBookField("04 FIELD PIC 9(10)V9(10).", StandardCharsets.UTF_8, bigDecimalFields, counters, indexes, occurs, paddingDefaults);
        assertEquals(20, field.size);
        assertEquals(CopyBookFieldType.DECIMAL, field.type);
    }

    // Signed decimal

    @org.junit.Test
    public void testIntTypeS9V99() throws Exception {
        CopyBookField field = new CopyBookField("04 FIELD PIC S9V9.", StandardCharsets.UTF_8, bigDecimalFields, counters, indexes, occurs, paddingDefaults);
        assertEquals(2, field.size);
        assertEquals(CopyBookFieldType.SIGNED_DECIMAL, field.type);
    }

    @org.junit.Test
    public void testIntTypeS99V99() throws Exception {
        CopyBookField field = new CopyBookField("04 FIELD PIC S99V99.", StandardCharsets.UTF_8, bigDecimalFields, counters, indexes, occurs, paddingDefaults);
        assertEquals(4, field.size);
        assertEquals(CopyBookFieldType.SIGNED_DECIMAL, field.type);
    }

    @org.junit.Test
    public void testIntTypeS91V9() throws Exception {
        CopyBookField field = new CopyBookField("04 FIELD PIC S9(1)V9.", StandardCharsets.UTF_8, bigDecimalFields, counters, indexes, occurs, paddingDefaults);
        assertEquals(2, field.size);
        assertEquals(CopyBookFieldType.SIGNED_DECIMAL, field.type);
    }

    @org.junit.Test
    public void testIntTypeS91V99() throws Exception {
        CopyBookField field = new CopyBookField("04 FIELD PIC S9(1)V99.", StandardCharsets.UTF_8, bigDecimalFields, counters, indexes, occurs, paddingDefaults);
        assertEquals(3, field.size);
        assertEquals(CopyBookFieldType.SIGNED_DECIMAL, field.type);
    }

    @org.junit.Test
    public void testIntTypeS91V91() throws Exception {
        CopyBookField field = new CopyBookField("04 FIELD PIC S9(1)V9(1).", StandardCharsets.UTF_8, bigDecimalFields, counters, indexes, occurs, paddingDefaults);
        assertEquals(2, field.size);
        assertEquals(CopyBookFieldType.SIGNED_DECIMAL, field.type);
    }

    @org.junit.Test
    public void testIntTypeS92V92() throws Exception {
        CopyBookField field = new CopyBookField("04 FIELD PIC S9(2)V9(2).", StandardCharsets.UTF_8, bigDecimalFields, counters, indexes, occurs, paddingDefaults);
        assertEquals(4, field.size);
        assertEquals(CopyBookFieldType.SIGNED_DECIMAL, field.type);
    }

    @org.junit.Test
    public void testIntTypeS910V910() throws Exception {
        CopyBookField field = new CopyBookField("04 FIELD PIC S9(10)V9(10).", StandardCharsets.UTF_8, bigDecimalFields, counters, indexes, occurs, paddingDefaults);
        assertEquals(20, field.size);
        assertEquals(CopyBookFieldType.SIGNED_DECIMAL, field.type);
    }
}