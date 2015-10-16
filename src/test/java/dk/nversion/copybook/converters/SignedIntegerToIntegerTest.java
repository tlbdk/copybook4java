package dk.nversion.copybook.converters;

import dk.nversion.copybook.CopyBookFieldSigningType;
import dk.nversion.copybook.exceptions.TypeConverterException;
import org.junit.Test;

import java.nio.charset.StandardCharsets;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;

public class SignedIntegerToIntegerTest {

    private TypeConverterBase typeConverter;

    public SignedIntegerToIntegerTest() {
        TypeConverterConfig config = new TypeConverterConfig();
        config.setCharset(StandardCharsets.UTF_8);
        config.setPaddingChar('0');
        config.setNullFillerChar((char) 0);
        config.setSigningType(CopyBookFieldSigningType.PREFIX);
        typeConverter = new SignedIntegerToInteger();
        typeConverter.setConfig(config);
    }

    public void testValidateSuccess() throws Exception {
        typeConverter.validate(Integer.TYPE, 2, -1);
    }

    @Test(expected = TypeConverterException.class)
    public void testValidateFail() throws Exception {
        typeConverter.validate(Long.TYPE, 2, -1);

    }

    @Test
    public void testTo() throws Exception {
        int result = (int)typeConverter.to(new byte[] { 48, 45, 49, 50 }, 0, 4, -1, true);
        assertEquals(-12, result);
    }

    @Test
    public void testFrom() throws Exception {
        byte[] bytes = typeConverter.from(-12, 4, -1, true);
        assertArrayEquals(new byte[] { 48, 45, 49, 50 }, bytes);

    }
}