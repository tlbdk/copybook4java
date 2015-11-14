package dk.nversion.copybook.converters;

import dk.nversion.copybook.exceptions.TypeConverterException;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import java.nio.charset.StandardCharsets;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;

public class StringToEnumTest {
    private TypeConverter typeConverter;
    private TypeConverterConfig config;

    public enum TestStringEnum implements TypeConverterStringEnum {
        HIGH("HH"), MEDIUM("MM"), LOW("LL");
        private final String value;

        TestStringEnum(String value) {
            this.value = value;
        }

        public String getValue() {
            return value;
        }
    }


    @Rule
    public ExpectedException expectedEx = ExpectedException.none();

    @Before
    public void runBeforeEveryTest() {
        this.config = new TypeConverterConfig();
        this.config.setCharset(StandardCharsets.UTF_8);
        this.config.setPaddingChar('0');
        this.config.setType(TestStringEnum.class);
        typeConverter = new StringToEnum();
        typeConverter.initialize(config);
    }

    @Test
    public void testValidateSuccess() throws Exception {
        typeConverter.validate(TestStringEnum.class, 2, -1);
    }

    @Test(expected = TypeConverterException.class)
    public void testValidateFail() throws Exception {
        typeConverter.validate(Long.TYPE, 2, -1);
    }

    @Test
    public void testTo() throws Exception {
        assertEquals(TestStringEnum.HIGH, typeConverter.to("HH".getBytes(StandardCharsets.UTF_8), 0, 2, -1, true));
        assertEquals(TestStringEnum.MEDIUM, typeConverter.to("MM".getBytes(StandardCharsets.UTF_8), 0, 2, -1, true));
        assertEquals(TestStringEnum.LOW, typeConverter.to("LL".getBytes(StandardCharsets.UTF_8), 0, 2, -1, true));
    }

    @Test
    public void testToNullDefaultValue() throws Exception {
        config.setNullFillerChar((char)0);
        config.setDefaultValue("MM");
        typeConverter.initialize(config);
        assertEquals(TestStringEnum.MEDIUM, typeConverter.to(new byte[4], 0, 2, 2, true));
    }

    @Test
    public void testToNullValue() throws Exception {
        expectedEx.expect(TypeConverterException.class);
        expectedEx.expectMessage("Unknown value for enum: null");
        config.setNullFillerChar((char)0);
        typeConverter.initialize(config);
        assertEquals(null, typeConverter.to(new byte[4], 0, 2, 2, true));
    }

    @Test
    public void testFrom() throws Exception {
        assertArrayEquals("HH".getBytes(StandardCharsets.UTF_8),  typeConverter.from(TestStringEnum.HIGH, 2, -1, true));
        assertArrayEquals("MM".getBytes(StandardCharsets.UTF_8),  typeConverter.from(TestStringEnum.MEDIUM, 2, -1, true));
        assertArrayEquals("LL".getBytes(StandardCharsets.UTF_8),  typeConverter.from(TestStringEnum.LOW, 2, -1, true));
    }

    @Test
    public void testFromOverflow() throws Exception {
        expectedEx.expect(TypeConverterException.class);
        expectedEx.expectMessage("Field to small for value");
        byte[] bytes = typeConverter.from(TestStringEnum.MEDIUM, 1, -1, true);
    }
}