package dk.nversion.copybook.converters;

import dk.nversion.copybook.exceptions.TypeConverterException;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import java.nio.charset.StandardCharsets;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

public class StringToStringTest {

    private TypeConverterBase typeConverter;

    @Rule
    public ExpectedException expectedEx = ExpectedException.none();

    public StringToStringTest() {
        TypeConverterConfig config = new TypeConverterConfig();
        config.setCharset(StandardCharsets.UTF_8);
        config.setPaddingChar(' ');
        config.setNullFillerChar((char)0);
        typeConverter = new StringToString();
        typeConverter.setConfig(config);
    }

    @Test
    public void testValidateSuccess() throws Exception {
        typeConverter.validate(String.class, 2, -1);
    }

    @Test(expected = TypeConverterException.class)
    public void testValidateFail() throws Exception {
        typeConverter.validate(Long.TYPE, 2, -1);
    }

    @Test
    public void testTo() throws Exception {
        assertEquals("ab12", typeConverter.to("ab12".getBytes(StandardCharsets.UTF_8), 0, 4, -1, true));
        assertEquals("12", typeConverter.to("  12".getBytes(StandardCharsets.UTF_8), 0, 4, -1, true));
    }

    @Test
    public void testToZeroValue() throws Exception {
        assertEquals("", typeConverter.to("    ".getBytes(StandardCharsets.UTF_8), 0, 4, -1, true));
    }

    @Test
    public void testToNullValue() throws Exception {
        assertNull(typeConverter.to(new byte[] { (byte)0, (byte)0 }, 0, 2, -1, true));
    }

    @Test
    public void testFrom() throws Exception {
        assertArrayEquals("ab12".getBytes(StandardCharsets.UTF_8), typeConverter.from("ab12", 4, -1, true));
        assertArrayEquals("  12".getBytes(StandardCharsets.UTF_8), typeConverter.from("12", 4, -1, true));
    }

    @Test
    public void testFromZeroValue() throws Exception {
        assertArrayEquals("    ".getBytes(StandardCharsets.UTF_8), typeConverter.from("", 4, -1, true));
    }

    @Test
    public void testFromNullValue() throws Exception {
        assertArrayEquals(new byte[] { (byte)0, (byte)0 }, typeConverter.from(null, 2, -1, true));
    }

    @Test
    public void testFromOverflow() throws Exception {
        expectedEx.expect(TypeConverterException.class);
        expectedEx.expectMessage("Field to small for value");
        byte[] bytes = typeConverter.from("12345", 4, -1, true);
    }
}