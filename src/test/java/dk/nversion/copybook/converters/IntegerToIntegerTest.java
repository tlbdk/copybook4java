package dk.nversion.copybook.converters;

import dk.nversion.copybook.exceptions.CopyBookException;
import dk.nversion.copybook.exceptions.TypeConverterException;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import java.nio.charset.StandardCharsets;

import static org.junit.Assert.*;

public class IntegerToIntegerTest {
    private TypeConverter typeConverter;
    private TypeConverterConfig config;

    @Rule
    public ExpectedException expectedEx = ExpectedException.none();

    @Before
    public void runBeforeEveryTest() throws CopyBookException {
        this.config = new TypeConverterConfig();
        this.config.setCharset(StandardCharsets.UTF_8);
        this.config.setPaddingChar('0');
        typeConverter = new IntegerToInteger();
        typeConverter.initialize(config);
    }


    @Test
    public void testValidateSuccess() throws Exception {
        typeConverter.validate(Integer.TYPE, 2, -1);
    }

    @Test(expected = TypeConverterException.class)
    public void testValidateFail() throws Exception {
        typeConverter.validate(Long.TYPE, 2, -1);
    }

    @Test
    public void testTo() throws Exception {
        assertEquals(0, (int)typeConverter.to("0".getBytes(StandardCharsets.UTF_8), 0, 1, -1, true));
        assertEquals(9, (int)typeConverter.to("9".getBytes(StandardCharsets.UTF_8), 0, 1, -1, true));
        assertEquals(10, (int)typeConverter.to("10".getBytes(StandardCharsets.UTF_8), 0, 2, -1, true));
        assertEquals(12, (int)typeConverter.to("0012".getBytes(StandardCharsets.UTF_8), 0, 4, -1, true));
    }

    @Test
    public void testToZeroValue() throws Exception {
        assertEquals(0, (int)typeConverter.to("00000000".getBytes(StandardCharsets.UTF_8), 0, 8, -1, true));
    }

    @Test
    public void testToNullDefaultValue() throws Exception {
        config.setNullFillerChar((char)0);
        config.setDefaultValue("42");
        typeConverter.initialize(config);
        assertEquals(42, (int)typeConverter.to(new byte[4], 0, 2, 2, true));
    }

    @Test
    public void testToNullValue() throws Exception {
        expectedEx.expect(NumberFormatException.class);
        expectedEx.expectMessage("For input string");
        config.setNullFillerChar((char)0);
        typeConverter.initialize(config);
        assertEquals(null, typeConverter.to(new byte[4], 0, 2, 2, true));
    }

    @Test
    public void testFrom() throws Exception {
        byte[] bytes = typeConverter.from(12, 4, -1, true);
        assertArrayEquals("0012".getBytes(StandardCharsets.UTF_8), bytes);
    }

    @Test
    public void testFromOverflow() throws Exception {
        expectedEx.expect(TypeConverterException.class);
        expectedEx.expectMessage("Field to small for value");
        byte[] bytes = typeConverter.from(12147, 4, -1, true);
    }
}