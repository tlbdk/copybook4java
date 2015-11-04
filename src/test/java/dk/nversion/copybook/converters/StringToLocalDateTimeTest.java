package dk.nversion.copybook.converters;

import dk.nversion.copybook.exceptions.CopyBookException;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import java.nio.charset.StandardCharsets;
import java.time.LocalDateTime;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;

public class StringToLocalDateTimeTest {
    private TypeConverter typeConverter;
    private TypeConverterConfig config;

    @Rule
    public ExpectedException expectedEx = ExpectedException.none();

    @Before
    public void runBeforeEveryTest() throws TypeConverterException {
        this.config = new TypeConverterConfig();
        this.config.setCharset(StandardCharsets.UTF_8);
        this.config.setPaddingChar('0');
        this.config.setFormat("yyyyMMddHHmmss");
        typeConverter = new StringToLocalDateTime();
        typeConverter.initialize(config);
    }


    @Test
    public void testValidateSuccess() throws Exception {
        typeConverter.validate(LocalDateTime.class, 2, -1);
    }

    @Test(expected = TypeConverterException.class)
    public void testValidateFail() throws Exception {
        typeConverter.validate(Long.TYPE, 2, -1);
    }

    @Test
    public void testTo() throws Exception {
        assertEquals(LocalDateTime.parse("2015-08-04T10:11:30"), typeConverter.to("20150804101130".getBytes(StandardCharsets.UTF_8), 0, 14, -1, true));
        assertEquals(LocalDateTime.parse("2015-08-04T10:11:30"), typeConverter.to("0020150804101130".getBytes(StandardCharsets.UTF_8), 0, 16, -1, true));
    }

    @Test
    public void testToZeroValue() throws Exception {
        assertEquals(null, typeConverter.to("00000000000000".getBytes(StandardCharsets.UTF_8), 0, 14, -1, true));
    }

    @Test
    public void testToNullDefaultValue() throws Exception {
        config.setNullFillerChar((char)0);
        config.setDefaultValue("20150804101130");
        typeConverter.initialize(config);
        assertEquals(LocalDateTime.parse("2015-08-04T10:11:30"), typeConverter.to(new byte[14], 0, 14, -1, true));
    }

    @Test
    public void testToNullValue() throws Exception {
        config.setNullFillerChar((char)0);
        typeConverter.initialize(config);
        assertEquals(null, typeConverter.to(new byte[14], 0, 14, -1, true));
    }

    @Test
    public void testFrom() throws Exception {
        byte[] bytes = typeConverter.from(LocalDateTime.parse("2015-08-04T10:11:30"), 16, -1, true);
        assertArrayEquals("0020150804101130".getBytes(StandardCharsets.UTF_8), bytes);
    }

    @Test
    public void testFromOverflow() throws Exception {
        expectedEx.expect(TypeConverterException.class);
        expectedEx.expectMessage("Field to small for value");
        byte[] bytes = typeConverter.from(LocalDateTime.parse("2015-08-04T10:11:30"), 4, -1, true);
    }
}