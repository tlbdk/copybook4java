package dk.nversion.copybook.converters;

import dk.nversion.copybook.exceptions.TypeConverterException;
import dk.nversion.copybook.serializers.CopyBookFieldSigningType;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import java.nio.charset.StandardCharsets;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;

public class SignedIntegerToIntegerLastByteBit8Test {
    private TypeConverter typeConverter;
    private TypeConverterConfig config;

    @Rule
    public ExpectedException expectedEx = ExpectedException.none();

    @Before
    public void runBeforeEveryTest() {
        this.config = new TypeConverterConfig();
        this.config.setCharset(StandardCharsets.UTF_8);
        this.config.setPaddingChar('0');
        config.setSigningType(CopyBookFieldSigningType.LAST_BYTE_BIT8);
        typeConverter = new SignedIntegerToInteger();
        typeConverter.initialize(config);
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
        assertEquals(0, (int)typeConverter.to("0".getBytes(StandardCharsets.UTF_8), 0, 1, -1, true));
        assertEquals(1, (int)typeConverter.to("1".getBytes(StandardCharsets.UTF_8), 0, 1, -1, true));
        assertEquals(5, (int)typeConverter.to("5".getBytes(StandardCharsets.UTF_8), 0, 1, -1, true));
        assertEquals(12, (int)typeConverter.to("12".getBytes(StandardCharsets.UTF_8), 0, 2, -1, true));
        assertEquals(-1, (int)typeConverter.to(new byte[] { (byte)177 }, 0, 1, -1, true));
        assertEquals(-5, (int)typeConverter.to(new byte[] { (byte)181 }, 0, 1, -1, true));
        assertEquals(-12, (int)typeConverter.to(new byte[] { (byte)49, (byte)178 }, 0, 2, -1, true));
    }

    @Test
    public void testToPadding() throws Exception {
        assertEquals(0, (int)typeConverter.to("00".getBytes(StandardCharsets.UTF_8), 0, 2, -1, true));
        assertEquals(1, (int)typeConverter.to("01".getBytes(StandardCharsets.UTF_8), 0, 2, -1, true));
        assertEquals(5, (int)typeConverter.to("05".getBytes(StandardCharsets.UTF_8), 0, 2, -1, true));
        assertEquals(12, (int)typeConverter.to("012".getBytes(StandardCharsets.UTF_8), 0, 3, -1, true));
        assertEquals(-1, (int)typeConverter.to(new byte[] { (byte)48, (byte)177 }, 0, 2, -1, true));
        assertEquals(-5, (int)typeConverter.to(new byte[] { (byte)48, (byte)181 }, 0, 2, -1, true));
        assertEquals(-12, (int)typeConverter.to(new byte[] { (byte)48, (byte)49, (byte)178 }, 0, 3, -1, true));
    }

    @Test
    public void testToZeroValue() throws Exception {
        assertEquals(0, (int)typeConverter.to("00000000".getBytes(StandardCharsets.UTF_8), 0, 8, -1, true));
    }

    @Test
    public void testToNullDefaultValue() throws Exception {
        config.setNullFillerChar((char)0);
        config.setDefaultValue("42");
        typeConverter.initialize(config);
        assertEquals(42, (int)typeConverter.to(new byte[4], 0, 2, 2, true));
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
        assertArrayEquals("0".getBytes(StandardCharsets.UTF_8), typeConverter.from(0, 1, -1, true));
        assertArrayEquals("1".getBytes(StandardCharsets.UTF_8), typeConverter.from(1, 1, -1, true));
        assertArrayEquals("5".getBytes(StandardCharsets.UTF_8), typeConverter.from(5, 1, -1, true));
        assertArrayEquals("12".getBytes(StandardCharsets.UTF_8), typeConverter.from(12, 2, -1, true));
        assertArrayEquals(new byte[] { (byte)177 }, typeConverter.from(-1, 1, -1, true));
        assertArrayEquals(new byte[] { (byte)181 }, typeConverter.from(-5, 1, -1, true));
        assertArrayEquals(new byte[] { (byte)49, (byte)178 }, typeConverter.from(-12, 2, -1, true));
    }

    @Test
    public void testFromPadding() throws Exception {
        assertArrayEquals("00".getBytes(StandardCharsets.UTF_8), typeConverter.from(0, 2, -1, true));
        assertArrayEquals("01".getBytes(StandardCharsets.UTF_8), typeConverter.from(1, 2, -1, true));
        assertArrayEquals("05".getBytes(StandardCharsets.UTF_8), typeConverter.from(5, 2, -1, true));
        assertArrayEquals("012".getBytes(StandardCharsets.UTF_8), typeConverter.from(12, 3, -1, true));
        assertArrayEquals(new byte[] { (byte)48, (byte)177 }, typeConverter.from(-1, 2, -1, true));
        assertArrayEquals(new byte[] { (byte)48, (byte)181 }, typeConverter.from(-5, 2, -1, true));
        assertArrayEquals(new byte[] { (byte)48, (byte)49, (byte)178 }, typeConverter.from(-12, 3, -1, true));
    }

    @Test
    public void testFromOverflow() throws Exception {
        expectedEx.expect(TypeConverterException.class);
        expectedEx.expectMessage("Field to small for value");
        byte[] bytes = typeConverter.from(1214, 3, -1, true);
    }

}