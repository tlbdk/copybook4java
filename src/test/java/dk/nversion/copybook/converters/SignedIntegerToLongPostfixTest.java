package dk.nversion.copybook.converters;

import dk.nversion.copybook.serializers.CopyBookFieldSigningType;
import dk.nversion.copybook.exceptions.CopyBookException;
import dk.nversion.copybook.exceptions.TypeConverterException;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import java.nio.charset.StandardCharsets;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;

public class SignedIntegerToLongPostfixTest {
    private TypeConverterBase typeConverter;
    private TypeConverterConfig config;

    @Rule
    public ExpectedException expectedEx = ExpectedException.none();

    @Before
    public void runBeforeEveryTest() throws CopyBookException {
        this.config = new TypeConverterConfig();
        this.config.setCharset(StandardCharsets.UTF_8);
        this.config.setPaddingChar('0');
        config.setSigningType(CopyBookFieldSigningType.POSTFIX);
        typeConverter = new SignedIntegerToLong();
        typeConverter.setConfig(config);
    }

    @Test
    public void testValidateSuccess() throws Exception {
        typeConverter.validate(Long.TYPE, 2, -1);
    }

    @Test(expected = TypeConverterException.class)
    public void testValidateFail() throws Exception {
        typeConverter.validate(Integer.TYPE, 2, -1);

    }

    @Test
    public void testTo() throws Exception {
        assertEquals(-12147483648L, (long)typeConverter.to("12147483648-".getBytes(StandardCharsets.UTF_8), 0, 12, -1, true));
        assertEquals(12147483648L, (long)typeConverter.to("12147483648+".getBytes(StandardCharsets.UTF_8), 0, 12, -1, true));
        assertEquals(9, (long)typeConverter.to("00000009+".getBytes(StandardCharsets.UTF_8), 0, 9, -1, true));
    }

    @Test
    public void testToZeroValue() throws Exception {
        assertEquals(0, (long)typeConverter.to("00000000+".getBytes(StandardCharsets.UTF_8), 0, 9, -1, true));
    }

    @Test
    public void testToNullDefaultValue() throws Exception {
        config.setNullFillerChar((char)0);
        config.setDefaultValue("42");
        typeConverter.setConfig(config);
        assertEquals(42, (long)typeConverter.to(new byte[4], 0, 2, 2, true));
    }

    @Test
    public void testToNullValue() throws Exception {
        expectedEx.expect(TypeConverterException.class);
        expectedEx.expectMessage("Missing sign char for value");
        config.setNullFillerChar((char)0);
        typeConverter.setConfig(config);
        assertEquals(null, typeConverter.to(new byte[4], 0, 2, 2, true));
    }

    @Test
    public void testFrom() throws Exception {
        assertArrayEquals("12147483648-".getBytes(StandardCharsets.UTF_8), typeConverter.from(-12147483648L, 12, -1, true));
        assertArrayEquals("12147483648+".getBytes(StandardCharsets.UTF_8), typeConverter.from(12147483648L, 12, -1, true));
    }

    @Test
    public void testToFailMissing() throws Exception {
        expectedEx.expect(TypeConverterException.class);
        expectedEx.expectMessage("Missing sign char for value");
        typeConverter.to("012147483648".getBytes(StandardCharsets.UTF_8), 0, 12, -1, true);
    }

    @Test
    public void testToFailWrongEnd() throws Exception {
        expectedEx.expect(TypeConverterException.class);
        expectedEx.expectMessage("Missing sign char for value");
        typeConverter.to("-12147483648".getBytes(StandardCharsets.UTF_8), 0, 12, -1, true);
    }

    @Test
    public void testFromOverflow() throws Exception {
        expectedEx.expect(TypeConverterException.class);
        expectedEx.expectMessage("Field to small for value");
        byte[] bytes = typeConverter.from(12147483648L, 4, -1, true);
    }

}