package dk.nversion.copybook.converters;

import dk.nversion.copybook.exceptions.CopyBookException;
import dk.nversion.copybook.exceptions.TypeConverterException;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import java.math.BigInteger;
import java.nio.charset.StandardCharsets;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;

public class IntegerToBigIntegerTest {
    private TypeConverterBase typeConverter;

    @Rule
    public ExpectedException expectedEx = ExpectedException.none();

    public IntegerToBigIntegerTest() throws CopyBookException {
        TypeConverterConfig config = new TypeConverterConfig();
        config.setCharset(StandardCharsets.UTF_8);
        config.setPaddingChar('0');
        typeConverter = new IntegerToBigInteger();
        typeConverter.setConfig(config);
    }

    @Test
    public void testValidateSuccess() throws Exception {
        typeConverter.validate(BigInteger.class, 10, -1);
    }

    @Test
    public void testValidateFail() throws Exception {
        expectedEx.expect(TypeConverterException.class);
        expectedEx.expectMessage("Only supports converting to and from BigInteger");
        typeConverter.validate(Integer.TYPE, 2, -1);

    }

    @Test
    public void testTo() throws Exception {
        BigInteger result = (BigInteger)typeConverter.to("12147483648".getBytes(StandardCharsets.UTF_8), 0, 11, -1, true);
        assertEquals(new BigInteger("12147483648"), result);
    }

    @Test
    public void testToZeroValue() throws Exception {
        assertEquals(new BigInteger("0"), typeConverter.to("0000000000".getBytes(StandardCharsets.UTF_8), 0, 10, -1, true));
    }

    @Test
    public void testFrom() throws Exception {
        byte[] bytes = typeConverter.from(new BigInteger("12147483648"), 11, -1, true);
        assertArrayEquals("12147483648".getBytes(StandardCharsets.UTF_8), bytes);
    }

    @Test
    public void testFromNullValue() throws Exception {
        // We default Null to 0 for big integer
        assertArrayEquals("00".getBytes(StandardCharsets.UTF_8), typeConverter.from(null, 2, -1, true));
    }

    @Test
    public void testFromOverflow() throws Exception {
        expectedEx.expect(TypeConverterException.class);
        expectedEx.expectMessage("Field to small for value");
        byte[] bytes = typeConverter.from(new BigInteger("12147483648"), 4, -1, true);
    }
}