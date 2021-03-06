/*
 * Copyright (c) 2015. Troels Liebe Bentsen <tlb@nversion.dk>
 * Licensed under the MIT license (LICENSE.txt)
 */

package dk.nversion.copybook.serializers;

import dk.nversion.copybook.CopyBookSerializer;
import dk.nversion.copybook.annotations.CopyBook;
import dk.nversion.copybook.annotations.CopyBookFieldFormat;
import dk.nversion.copybook.annotations.CopyBookLine;
import dk.nversion.copybook.converters.StringToString;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;

public class CopyBookMapperFullCharsetTest {

    @CopyBook(charset = "cp037", type = FullMapper.class)
    @CopyBookFieldFormat(type = StringToString.class, paddingChar = ' ', nullFillerChar = (byte)0, signingType = CopyBookFieldSigningType.PREFIX, rightPadding = false)
    static public class fieldTypeStringCp037 {
        @CopyBookLine("01 FIELD PIC X(4).")
        public String field;
    }

    @org.junit.Test
    public void testFieldTypeStringCp037() throws Exception {
        CopyBookSerializer serializer = new CopyBookSerializer(fieldTypeStringCp037.class);
        fieldTypeStringCp037 test = new fieldTypeStringCp037();
        test.field = "ok";
        byte[] testBytes = serializer.serialize(test);
        assertArrayEquals(testBytes, "  ok".getBytes("cp037"));
        fieldTypeStringCp037 test2 = serializer.deserialize(testBytes, fieldTypeStringCp037.class);
        assertEquals("ok", test2.field);
    }
}