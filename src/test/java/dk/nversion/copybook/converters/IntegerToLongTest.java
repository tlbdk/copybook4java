package dk.nversion.copybook.converters;

import dk.nversion.copybook.exceptions.TypeConverterException;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import java.nio.charset.StandardCharsets;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;

public class IntegerToLongTest {
    private TypeConverterBase typeConverter;

    @Rule
    public ExpectedException expectedEx = ExpectedException.none();

    public IntegerToLongTest() {
        TypeConverterConfig config = new TypeConverterConfig();
        config.setCharset(StandardCharsets.UTF_8);
        config.setPaddingChar('0');
        typeConverter = new IntegerToLong();
        typeConverter.setConfig(config);
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