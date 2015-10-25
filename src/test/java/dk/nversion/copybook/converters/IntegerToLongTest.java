package dk.nversion.copybook.converters;

import dk.nversion.copybook.exceptions.CopyBookException;
import dk.nversion.copybook.exceptions.TypeConverterException;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import java.nio.charset.StandardCharsets;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;

public class IntegerToLongTest {
    private TypeConverter typeConverter;
    private TypeConverterConfig config;

    @Rule
    public ExpectedException expectedEx = ExpectedException.none();

    @Before
    public void runBeforeEveryTest() throws CopyBookException {
        this.config = new TypeConverterConfig();
        this.config.setCharset(StandardCharsets.UTF_8);
        this.config.setPaddingChar('0');
        typeConverter = new IntegerToLong();
        typeConverter.initialize(config);
    }


    @Test
    public void testValidateSuccess() throws Exception {
        typeConverter.validate(Long.TYPE, 10, -1);
    }

    @Test(expected = TypeConverterException.class)
    public void testValidateFail() throws Exception {
        typeConverter.validate(Integer.TYPE, 2, -1);

    }

    @Test
    public void testTo() throws Exception {
        long result = (long)typeConverter.to("12147483648".getBytes(StandardCharsets.UTF_8), 0, 11, -1, true);
        assertEquals(12147483648L, result);
    }

    @Test
    public void testToZeroValue() throws Exception {
        assertEquals(0, (long)typeConverter.to("0000000000".getBytes(StandardCharsets.UTF_8), 0, 10, -1, true));
    }

    @Test
    public void testToNullDefaultValue() throws Exception {
        config.setNullFillerChar((char)0);
        config.setDefaultValue("42");
        typeConverter.initialize(config);
        assertEquals(42, (long)typeConverter.to(new byte[4], 0, 2, 2, true));
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
        byte[] bytes = typeConverter.from(12147483648L, 11, -1, true);
        assertArrayEquals("12147483648".getBytes(StandardCharsets.UTF_8), bytes);
    }

    @Test
    public void testFromOverflow() throws Exception {
        expectedEx.expect(TypeConverterException.class);
        expectedEx.expectMessage("Field to small for value");
        byte[] bytes = typeConverter.from(12147483648L, 4, -1, true);
    }
}