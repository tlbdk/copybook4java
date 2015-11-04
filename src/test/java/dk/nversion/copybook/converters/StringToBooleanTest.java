package dk.nversion.copybook.converters;

import dk.nversion.copybook.exceptions.CopyBookException;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import java.nio.charset.StandardCharsets;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;

// TODO: Implement StringToBoolean
@Ignore
public class StringToBooleanTest {
    private TypeConverter typeConverter;
    private TypeConverterConfig config;

    @Rule
    public ExpectedException expectedEx = ExpectedException.none();

    @Before
    public void runBeforeEveryTest() throws TypeConverterException {
        this.config = new TypeConverterConfig();
        this.config.setCharset(StandardCharsets.UTF_8);
        this.config.setPaddingChar('0');
        this.config.setFormat("Y|N");
        typeConverter = new IntegerToBoolean();
        typeConverter.initialize(config);
    }

    @Test
    public void testValidateSuccess() throws Exception {
        //typeConverter.validate(Integer.TYPE, 2, -1);
    }

    @Test(expected = TypeConverterException.class)
    public void testValidateFail() throws Exception {
        //typeConverter.validate(Long.TYPE, 2, -1);
    }

    @Test
    public void testTo() throws Exception {
        assertEquals(true, typeConverter.to("N".getBytes(StandardCharsets.UTF_8), 0, 1, -1, true));
        assertEquals(false, typeConverter.to("Y".getBytes(StandardCharsets.UTF_8), 0, 1, -1, true));
    }

    @Test
    public void testToNullDefaultValue() throws Exception {
        config.setNullFillerChar((char)0);
        config.setDefaultValue("Y");
        typeConverter.initialize(config);
        assertEquals(true, typeConverter.to(new byte[4], 0, 2, 2, true));
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
        assertArrayEquals("Y".getBytes(StandardCharsets.UTF_8),  typeConverter.from(true, 1, -1, true));
        assertArrayEquals("N".getBytes(StandardCharsets.UTF_8),  typeConverter.from(false, 1, -1, true));
    }

    @Test
    public void testFromOverflow() throws Exception {
        expectedEx.expect(TypeConverterException.class);
        expectedEx.expectMessage("Field to small for value");
        byte[] bytes = typeConverter.from(12147, 4, -1, true);
    }
}