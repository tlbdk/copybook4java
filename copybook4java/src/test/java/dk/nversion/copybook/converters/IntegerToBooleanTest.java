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

import java.nio.charset.StandardCharsets;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;

public class IntegerToBooleanTest {
    private TypeConverter typeConverter;
    private TypeConverterConfig config;

    @Rule
    public ExpectedException expectedEx = ExpectedException.none();

    @Before
    public void runBeforeEveryTest() {
        this.config = new TypeConverterConfig();
        this.config.setCharset(StandardCharsets.UTF_8);
        this.config.setPaddingChar('0');
        typeConverter = new IntegerToBoolean();
        typeConverter.initialize(config);
    }


    @Test
    public void testValidateSuccess() throws Exception {
        typeConverter.validate(Boolean.TYPE, 2, -1);
    }

    @Test(expected = TypeConverterException.class)
    public void testValidateFail() throws Exception {
        typeConverter.validate(Long.TYPE, 2, -1);
    }

    @Test
    public void testTo() throws Exception {
        assertEquals(false, typeConverter.to("0".getBytes(StandardCharsets.UTF_8), 0, 1, -1, true));
        assertEquals(true, typeConverter.to("1".getBytes(StandardCharsets.UTF_8), 0, 1, -1, true));
    }

    @Test
    public void testToNullDefaultValue() throws Exception {
        config.setNullFillerChar((char)0);
        config.setDefaultValue("1");
        typeConverter.initialize(config);
        assertEquals(true, typeConverter.to(new byte[4], 0, 2, 2, true));
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
        assertArrayEquals("1".getBytes(StandardCharsets.UTF_8),  typeConverter.from(true, 1, -1, true));
        assertArrayEquals("0".getBytes(StandardCharsets.UTF_8),  typeConverter.from(false, 1, -1, true));
    }
}