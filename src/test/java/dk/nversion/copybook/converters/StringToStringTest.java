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
import static org.junit.Assert.assertNull;

public class StringToStringTest {
    private TypeConverter typeConverter;
    private TypeConverterConfig config;

    @Rule
    public ExpectedException expectedEx = ExpectedException.none();

    @Before
    public void runBeforeEveryTest() {
        this.config = new TypeConverterConfig();
        this.config.setCharset(StandardCharsets.UTF_8);
        this.config.setPaddingChar(' ');
        config.setNullFillerChar((char)0);
        typeConverter = new StringToString();
        typeConverter.initialize(config);
    }

    @Test
    public void testValidateSuccess() throws Exception {
        typeConverter.validate(String.class, 2, -1);
    }

    @Test(expected = TypeConverterException.class)
    public void testValidateFail() throws Exception {
        typeConverter.validate(Long.TYPE, 2, -1);
    }

    @Test
    public void testTo() throws Exception {
        assertEquals("k", typeConverter.to("k".getBytes(StandardCharsets.UTF_8), 0, 1, -1, true));
        assertEquals("ok", typeConverter.to("ok".getBytes(StandardCharsets.UTF_8), 0, 2, -1, true));
        assertEquals("ab12", typeConverter.to("ab12".getBytes(StandardCharsets.UTF_8), 0, 4, -1, true));
        assertEquals("12", typeConverter.to("  12".getBytes(StandardCharsets.UTF_8), 0, 4, -1, true));
    }

    @Test
    public void testToZeroValue() throws Exception {
        assertEquals("", typeConverter.to("    ".getBytes(StandardCharsets.UTF_8), 0, 4, -1, true));
    }

    @Test
    public void testToNullValue() throws Exception {
        assertNull(typeConverter.to(new byte[] { (byte)0, (byte)0 }, 0, 2, -1, true));
    }

    @Test
    public void testToNullDefaultValue() throws Exception {
        config.setNullFillerChar((char)0);
        config.setDefaultValue("42");
        typeConverter.initialize(config);
        assertEquals("42", typeConverter.to(new byte[4], 0, 2, 2, true));
    }

    @Test
    public void testFrom() throws Exception {
        assertArrayEquals("ab12".getBytes(StandardCharsets.UTF_8), typeConverter.from("ab12", 4, -1, true));
        assertArrayEquals("  12".getBytes(StandardCharsets.UTF_8), typeConverter.from("12", 4, -1, true));
    }

    @Test
    public void testFromZeroValue() throws Exception {
        assertArrayEquals("    ".getBytes(StandardCharsets.UTF_8), typeConverter.from("", 4, -1, true));
    }

    @Test
    public void testFromNullValue() throws Exception {
        assertArrayEquals(new byte[] { (byte)0, (byte)0 }, typeConverter.from(null, 2, -1, true));
    }

    @Test
    public void testFromOverflow() throws Exception {
        expectedEx.expect(TypeConverterException.class);
        expectedEx.expectMessage("Field to small for value");
        byte[] bytes = typeConverter.from("12345", 4, -1, true);
    }
}