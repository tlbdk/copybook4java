package dk.nversion.copybook.converters;

import dk.nversion.copybook.CopyBookFieldSigningType;
import dk.nversion.copybook.exceptions.TypeConverterException;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import java.math.BigInteger;
import java.nio.charset.StandardCharsets;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;

public class SignedIntegerToBigIntegerPrefixTest {
    private TypeConverterBase typeConverter;

    @Rule
    public ExpectedException expectedEx = ExpectedException.none();

    public SignedIntegerToBigIntegerPrefixTest() {
        TypeConverterConfig config = new TypeConverterConfig();
        config.setCharset(StandardCharsets.UTF_8);
        config.setPaddingChar('0');
        config.setSigningType(CopyBookFieldSigningType.PREFIX);
        typeConverter = new SignedIntegerToBigInteger();
        typeConverter.setConfig(config);
    }


    @Test
    public void testValidateSuccess() throws Exception {
        typeConverter.validate(BigInteger.class, 2, -1);
    }

    @Test(expected = TypeConverterException.class)
    public void testValidateFail() throws Exception {
        typeConverter.validate(Integer.TYPE, 2, -1);

    }

    @Test
    public void testTo() throws Exception {
        assertEquals(new BigInteger("-12147483648"), typeConverter.to("0-12147483648".getBytes(StandardCharsets.UTF_8), 0, 13, -1, true));
        assertEquals(new BigInteger("12147483648"), typeConverter.to("0+12147483648".getBytes(StandardCharsets.UTF_8), 0, 13, -1, true));
        assertEquals(new BigInteger("12"), typeConverter.to("000000+12".getBytes(StandardCharsets.UTF_8), 0, 9, -1, true));
    }

    @Test
    public void testToZeroValue() throws Exception {
        assertEquals(new BigInteger("0"), typeConverter.to("+00000000".getBytes(StandardCharsets.UTF_8), 0, 9, -1, true));
    }

    @Test
    public void testFrom() throws Exception {
        assertArrayEquals("0-12147483648".getBytes(StandardCharsets.UTF_8), typeConverter.from(new BigInteger("-12147483648"), 13, -1, true));
        assertArrayEquals("0+12147483648".getBytes(StandardCharsets.UTF_8), typeConverter.from(new BigInteger("12147483648"), 13, -1, true));
    }

    @Test
    public void testFromNullValue() throws Exception {
        // We default Null to 0 for big integer
        assertArrayEquals("+0".getBytes(StandardCharsets.UTF_8), typeConverter.from(null, 2, -1, true));
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
        typeConverter.to("12147483648-".getBytes(StandardCharsets.UTF_8), 0, 12, -1, true);
    }

    @Test
    public void testFromOverflow() throws Exception {
        expectedEx.expect(TypeConverterException.class);
        expectedEx.expectMessage("Field to small for value");
        byte[] bytes = typeConverter.from(new BigInteger("12147483648"), 4, -1, true);
    }

}