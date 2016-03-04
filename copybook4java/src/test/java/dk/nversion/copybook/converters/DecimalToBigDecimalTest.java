/*
 * Copyright (c) 2015. Troels Liebe Bentsen <tlb@nversion.dk>
 * Licensed under the MIT license (LICENSE.txt)
 */

package dk.nversion.copybook.converters;

import dk.nversion.copybook.exceptions.TypeConverterException;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import java.math.BigDecimal;
import java.nio.charset.StandardCharsets;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;

public class DecimalToBigDecimalTest {
    private TypeConverter typeConverter;
    private TypeConverterConfig config;

    @Rule
    public ExpectedException expectedEx = ExpectedException.none();

    @Before
    public void runBeforeEveryTest() {
        this.config = new TypeConverterConfig();
        this.config.setCharset(StandardCharsets.UTF_8);
        this.config.setPaddingChar('0');
        typeConverter = new DecimalToBigDecimal();
        typeConverter.initialize(config);
    }

    @Test
    public void testValidateSuccess() throws Exception {
        typeConverter.validate(BigDecimal.class, 10, -1);
    }

    @Test
    public void testValidateTyopeFail() throws Exception {
        expectedEx.expect(TypeConverterException.class);
        expectedEx.expectMessage("Only supports converting to and from BigDecimal");
        typeConverter.validate(Integer.TYPE, 2, -1);
    }

    @Test
    public void testValidateFail() throws Exception {
        expectedEx.expect(TypeConverterException.class);
        expectedEx.expectMessage("Field to small to hold a decimal number");
        typeConverter.validate(BigDecimal.class, 1, -1);
    }

    @Test
    public void testTo() throws Exception {
        assertEquals(new BigDecimal("1.1"), typeConverter.to("11".getBytes(StandardCharsets.UTF_8), 0, 2, 1, true));
        assertEquals(new BigDecimal("10.01"), typeConverter.to("1001".getBytes(StandardCharsets.UTF_8), 0, 4, 2, true));
        assertEquals(new BigDecimal("10.00001"), typeConverter.to("1000001".getBytes(StandardCharsets.UTF_8), 0, 7, 5, true));
        assertEquals(new BigDecimal("10.10000"), typeConverter.to("1010000".getBytes(StandardCharsets.UTF_8), 0, 7, 5, true));
        assertEquals(new BigDecimal("101000.0"), typeConverter.to("1010000".getBytes(StandardCharsets.UTF_8), 0, 7, 1, true));
    }

    @Test
    public void testToSubOne() throws Exception {
        assertEquals(new BigDecimal("0.01"), typeConverter.to("0001".getBytes(StandardCharsets.UTF_8), 0, 4, 2, true));
        assertEquals(new BigDecimal("0.10"), typeConverter.to("0010".getBytes(StandardCharsets.UTF_8), 0, 4, 2, true));
        assertEquals(new BigDecimal("0.00001"), typeConverter.to("0000001".getBytes(StandardCharsets.UTF_8), 0, 7, 5, true));
        assertEquals(new BigDecimal("0.01100"), typeConverter.to("0001100".getBytes(StandardCharsets.UTF_8), 0, 7, 5, true));
    }

    @Test
    public void testToZeroValue() throws Exception {
        assertEquals(new BigDecimal("0.00"), typeConverter.to("0000000000".getBytes(StandardCharsets.UTF_8), 0, 9, 2, true));
    }

    @Test
    public void testToNullDefaultValue() throws Exception {
        config.setNullFillerChar((char)0);
        config.setDefaultValue("42.00");
        typeConverter.initialize(config);
        assertEquals(new BigDecimal("42.00"), typeConverter.to(new byte[4], 0, 2, 2, true));
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
        assertArrayEquals("1001".getBytes(StandardCharsets.UTF_8), typeConverter.from(new BigDecimal("10.01"), 4, 2, true));
    }

    @Test
    public void testFromNullValue() throws Exception {
        // We default Null to 0 for big integer
        config.setDefaultValue("0");
        typeConverter.initialize(config);
        assertArrayEquals("0000".getBytes(StandardCharsets.UTF_8), typeConverter.from(null, 4, 2, true));
    }

    @Test
    public void testFromOverflow() throws Exception {
        expectedEx.expect(TypeConverterException.class);
        expectedEx.expectMessage("Field to small for value");
        byte[] bytes = typeConverter.from(new BigDecimal("12147483648.00"), 4, 2, true);
    }
}