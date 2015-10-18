package dk.nversion.copybook.converters;

import dk.nversion.copybook.CopyBookFieldSigningType;
import dk.nversion.copybook.exceptions.CopyBookException;
import dk.nversion.copybook.exceptions.TypeConverterException;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import java.nio.charset.StandardCharsets;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;

public class SignedIntegerToIntegerPrefixTest {
    private TypeConverterBase typeConverter;

    @Rule
    public ExpectedException expectedEx = ExpectedException.none();

    public SignedIntegerToIntegerPrefixTest() throws CopyBookException {
        TypeConverterConfig config = new TypeConverterConfig();
        config.setCharset(StandardCharsets.UTF_8);
        config.setPaddingChar('0');
        config.setSigningType(CopyBookFieldSigningType.PREFIX);
        typeConverter = new SignedIntegerToInteger();
        typeConverter.setConfig(config);
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
        assertEquals(-2, (int)typeConverter.to("-2".getBytes(StandardCharsets.UTF_8), 0, 2, -1, true));
        assertEquals(-12, (int)typeConverter.to("-12".getBytes(StandardCharsets.UTF_8), 0, 3, -1, true));
        assertEquals(10, (int)typeConverter.to("+10".getBytes(StandardCharsets.UTF_8), 0, 3, -1, true));
        assertEquals(-12, (int)typeConverter.to("0-12".getBytes(StandardCharsets.UTF_8), 0, 4, -1, true));
        assertEquals(12, (int)typeConverter.to("0+12".getBytes(StandardCharsets.UTF_8), 0, 4, -1, true));
        assertEquals(12, (int)typeConverter.to("000000+12".getBytes(StandardCharsets.UTF_8), 0, 9, -1, true));
    }

    @Test
    public void testToZeroValue() throws Exception {
        assertEquals(0, (int)typeConverter.to("+00000000".getBytes(StandardCharsets.UTF_8), 0, 9, -1, true));
    }

    @Test
    public void testFrom() throws Exception {
        assertArrayEquals("0-12".getBytes(StandardCharsets.UTF_8), typeConverter.from(-12, 4, -1, true));
        assertArrayEquals("0+12".getBytes(StandardCharsets.UTF_8), typeConverter.from(12, 4, -1, true));
    }

    @Test
    public void testToFailMissing() throws Exception {
        expectedEx.expect(TypeConverterException.class);
        expectedEx.expectMessage("Missing sign char for value");
        typeConverter.to("0012".getBytes(StandardCharsets.UTF_8), 0, 4, -1, true);
    }

    @Test
    public void testToFailWrongEnd() throws Exception {
        expectedEx.expect(TypeConverterException.class);
        expectedEx.expectMessage("Missing sign char for value");
        typeConverter.to("012-".getBytes(StandardCharsets.UTF_8), 0, 4, -1, true);
    }

    @Test
    public void testFromOverflow() throws Exception {
        expectedEx.expect(TypeConverterException.class);
        expectedEx.expectMessage("Field to small for value");
        byte[] bytes = typeConverter.from(1214, 4, -1, true);
    }

}