package dk.nversion.copybook.converters;

import dk.nversion.copybook.serializers.CopyBookFieldSigningType;
import dk.nversion.copybook.exceptions.CopyBookException;
import dk.nversion.copybook.exceptions.TypeConverterException;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import java.nio.charset.Charset;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;

public class SignedIntegerToIntegerLastByteEBCDICBit5Test {
    private TypeConverterBase typeConverter;
    private Charset charset;

    @Rule
    public ExpectedException expectedEx = ExpectedException.none();

    public SignedIntegerToIntegerLastByteEBCDICBit5Test() throws CopyBookException {
        this.charset = Charset.forName("cp037");
        TypeConverterConfig config = new TypeConverterConfig();
        config.setCharset(this.charset);
        config.setPaddingChar('0');
        config.setSigningType(CopyBookFieldSigningType.LAST_BYTE_EBCDIC_BIT5);
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
        assertEquals(0, (int)typeConverter.to(new byte[] { (byte)192 }, 0, 1, -1, true));
        assertEquals(1, (int)typeConverter.to(new byte[] { (byte)193 }, 0, 1, -1, true));
        assertEquals(5, (int)typeConverter.to(new byte[] { (byte)197 }, 0, 1, -1, true));
        assertEquals(12, (int)typeConverter.to(new byte[] { (byte)241, (byte)194 }, 0, 2, -1, true));
        assertEquals(-1, (int)typeConverter.to(new byte[] { (byte)209 }, 0, 1, -1, true));
        assertEquals(-5, (int)typeConverter.to(new byte[] { (byte)213 }, 0, 1, -1, true));
        assertEquals(-12, (int)typeConverter.to(new byte[] { (byte)241, (byte)210 }, 0, 2, -1, true));
    }

    @Test
    public void testToPadding() throws Exception {
        assertEquals(0, (int)typeConverter.to(new byte[] { (byte)240, (byte)192 }, 0, 2, -1, true));
        assertEquals(1, (int)typeConverter.to(new byte[] { (byte)240, (byte)193 }, 0, 2, -1, true));
        assertEquals(5, (int)typeConverter.to(new byte[] { (byte)240, (byte)197 }, 0, 2, -1, true));
        assertEquals(12, (int)typeConverter.to(new byte[] { (byte)240, (byte)241, (byte)194 }, 0, 3, -1, true));
        assertEquals(-1, (int)typeConverter.to(new byte[] { (byte)240, (byte)209 }, 0, 2, -1, true));
        assertEquals(-5, (int)typeConverter.to(new byte[] { (byte)240, (byte)213 }, 0, 2, -1, true));
        assertEquals(-12, (int)typeConverter.to(new byte[] { (byte)240, (byte)241, (byte)210 }, 0, 3, -1, true));
    }

    @Test
    public void testToZeroValue() throws Exception {
        assertEquals(0, (int)typeConverter.to("00000000".getBytes(this.charset), 0, 8, -1, true));
    }

    @Test
    public void testFrom() throws Exception {
        assertArrayEquals(new byte[] { (byte)192 }, typeConverter.from(0, 1, -1, true));
        assertArrayEquals(new byte[] { (byte)193 }, typeConverter.from(1, 1, -1, true));
        assertArrayEquals(new byte[] { (byte)197 }, typeConverter.from(5, 1, -1, true));
        assertArrayEquals(new byte[] { (byte)241, (byte)194 }, typeConverter.from(12, 2, -1, true));
        assertArrayEquals(new byte[] { (byte)209 }, typeConverter.from(-1, 1, -1, true));
        assertArrayEquals(new byte[] { (byte)213 }, typeConverter.from(-5, 1, -1, true));
        assertArrayEquals(new byte[] { (byte)241, (byte)210 }, typeConverter.from(-12, 2, -1, true));
    }

    @Test
    public void testFromPadding() throws Exception {
        assertArrayEquals(new byte[] { (byte)240, (byte)192 }, typeConverter.from(0, 2, -1, true));
        assertArrayEquals(new byte[] { (byte)240, (byte)193 }, typeConverter.from(1, 2, -1, true));
        assertArrayEquals(new byte[] { (byte)240, (byte)197 }, typeConverter.from(5, 2, -1, true));
        assertArrayEquals(new byte[] { (byte)240, (byte)241, (byte)194 }, typeConverter.from(12, 3, -1, true));
        assertArrayEquals(new byte[] { (byte)240, (byte)209 }, typeConverter.from(-1, 2, -1, true));
        assertArrayEquals(new byte[] { (byte)240, (byte)213 }, typeConverter.from(-5, 2, -1, true));
        assertArrayEquals(new byte[] { (byte)240, (byte)241, (byte)210 }, typeConverter.from(-12, 3, -1, true));
    }


    @Test
    public void testFromOverflow() throws Exception {
        expectedEx.expect(TypeConverterException.class);
        expectedEx.expectMessage("Field to small for value");
        byte[] bytes = typeConverter.from(1214, 3, -1, true);
    }

}