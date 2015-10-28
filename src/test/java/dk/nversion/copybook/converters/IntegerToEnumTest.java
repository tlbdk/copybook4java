package dk.nversion.copybook.converters;

import dk.nversion.copybook.exceptions.CopyBookException;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import java.nio.charset.StandardCharsets;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;

public class IntegerToEnumTest {
    private TypeConverter typeConverter;
    private TypeConverterConfig config;

    public enum TestEnum implements TypeConverterEnum {
        HIGH(30), MEDIUM(20), LOW(10);
        private final int value;

        TestEnum(int value) {
            this.value = value;
        }

        public int getValue() {
            return value;
        }
    }


    @Rule
    public ExpectedException expectedEx = ExpectedException.none();

    @Before
    public void runBeforeEveryTest() throws CopyBookException {
        this.config = new TypeConverterConfig();
        this.config.setCharset(StandardCharsets.UTF_8);
        this.config.setPaddingChar('0');
        this.config.setType(TestEnum.class);
        typeConverter = new IntegerToEnum();
        typeConverter.initialize(config);
    }

    @Test
    public void testValidateSuccess() throws Exception {
        typeConverter.validate(TestEnum.class, 2, -1);
    }

    @Test(expected = TypeConverterException.class)
    public void testValidateFail() throws Exception {
        typeConverter.validate(Long.TYPE, 2, -1);
    }

    @Test
    public void testTo() throws Exception {
        assertEquals(TestEnum.HIGH, typeConverter.to("30".getBytes(StandardCharsets.UTF_8), 0, 2, -1, true));
        assertEquals(TestEnum.MEDIUM, typeConverter.to("20".getBytes(StandardCharsets.UTF_8), 0, 2, -1, true));
        assertEquals(TestEnum.LOW, typeConverter.to("10".getBytes(StandardCharsets.UTF_8), 0, 2, -1, true));
    }

    @Test
    public void testToNullDefaultValue() throws Exception {
        config.setNullFillerChar((char)0);
        config.setDefaultValue("10");
        typeConverter.initialize(config);
        assertEquals(TestEnum.LOW, typeConverter.to(new byte[4], 0, 2, 2, true));
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
        assertArrayEquals("30".getBytes(StandardCharsets.UTF_8),  typeConverter.from(TestEnum.HIGH, 2, -1, true));
        assertArrayEquals("20".getBytes(StandardCharsets.UTF_8),  typeConverter.from(TestEnum.MEDIUM, 2, -1, true));
        assertArrayEquals("10".getBytes(StandardCharsets.UTF_8),  typeConverter.from(TestEnum.LOW, 2, -1, true));
    }

    @Test
    public void testFromOverflow() throws Exception {
        expectedEx.expect(TypeConverterException.class);
        expectedEx.expectMessage("Field to small for value");
        byte[] bytes = typeConverter.from(TestEnum.MEDIUM, 1, -1, true);
    }
}