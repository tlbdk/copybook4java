/*
 * Copyright (c) 2015. Troels Liebe Bentsen <tlb@nversion.dk>
 * Licensed under the MIT license (LICENSE.txt)
 */

package dk.nversion.copybook.serializers;

import dk.nversion.copybook.CopyBookSerializer;
import dk.nversion.copybook.annotations.CopyBook;
import dk.nversion.copybook.annotations.CopyBookLine;

import java.math.BigDecimal;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;

public class CopyBookMapperFullOccursPicSingleLineTest {

    @CopyBook(type = FullMapper.class)
    static public class StringFieldOccursTwoTimes {
        @CopyBookLine("01 FIELDS OCCURS 2 TIMES PIC X(4).")
        public String[] fields;
    }

    @CopyBook(type = FullMapper.class)
    static public class IntFieldOccursTwoTimes {
        @CopyBookLine("01 FIELDS OCCURS 2 TIMES PIC 9(4).")
        public int[] fields;
    }

    @CopyBook(type = FullMapper.class)
    static public class SignedIntFieldOccursTwoTimes {
        @CopyBookLine("01 FIELDS OCCURS 2 TIMES PIC S9(4).")
        public int[] fields;
    }

    @CopyBook(type = FullMapper.class)
    static public class DecimalFieldOccursTwoTimes {
        @CopyBookLine("01 FIELDS OCCURS 2 TIMES PIC 9(3)V9(2).")
        public BigDecimal[] fields;
    }

    @CopyBook(type = FullMapper.class)
    static public class SignedDecimalFieldOccursTwoTimes {
        @CopyBookLine("01 FIELDS OCCURS 2 TIMES PIC S9(3)V9(2).")
        public BigDecimal[] fields;
    }

    @org.junit.Test
      public void testStringFieldOccursTwoTimes() throws Exception {
        CopyBookSerializer serializer = new CopyBookSerializer(StringFieldOccursTwoTimes.class);
        StringFieldOccursTwoTimes test = new StringFieldOccursTwoTimes();
        test.fields = new String[] {"abcd", "1234"};
        StringFieldOccursTwoTimes test2 = serializer.deserialize(serializer.serialize(test), StringFieldOccursTwoTimes.class);
        assertArrayEquals(test.fields, test2.fields);
    }

    @org.junit.Test
    public void testIntFieldOccursTwoTimes() throws Exception {
        CopyBookSerializer serializer = new CopyBookSerializer(IntFieldOccursTwoTimes.class);
        IntFieldOccursTwoTimes test = new IntFieldOccursTwoTimes();
        test.fields = new int[] {1, 2};
        IntFieldOccursTwoTimes test2 = serializer.deserialize(serializer.serialize(test), IntFieldOccursTwoTimes.class);
        assertArrayEquals(test.fields, test2.fields);
    }

    @org.junit.Test
    public void testSignedIntFieldOccursTwoTimes() throws Exception {
        CopyBookSerializer serializer = new CopyBookSerializer(SignedIntFieldOccursTwoTimes.class);
        SignedIntFieldOccursTwoTimes test = new SignedIntFieldOccursTwoTimes();
        test.fields = new int[] {-1, -2};
        SignedIntFieldOccursTwoTimes test2 = serializer.deserialize(serializer.serialize(test), SignedIntFieldOccursTwoTimes.class);
        assertArrayEquals(test.fields, test2.fields);
    }

    @org.junit.Test
    public void testDecimalFieldOccursTwoTimes() throws Exception {
        CopyBookSerializer serializer = new CopyBookSerializer(DecimalFieldOccursTwoTimes.class);
        DecimalFieldOccursTwoTimes test = new DecimalFieldOccursTwoTimes();
        test.fields = new BigDecimal[] {new BigDecimal("1.00"), new BigDecimal("2.00")};
        DecimalFieldOccursTwoTimes test2 = serializer.deserialize(serializer.serialize(test), DecimalFieldOccursTwoTimes.class);
        assertArrayEquals(test.fields, test2.fields);
    }

    @org.junit.Test
    public void testSignedDecimalFieldOccursTwoTimes() throws Exception {
        CopyBookSerializer serializer = new CopyBookSerializer(SignedDecimalFieldOccursTwoTimes.class);
        SignedDecimalFieldOccursTwoTimes test = new SignedDecimalFieldOccursTwoTimes();
        test.fields = new BigDecimal[] {new BigDecimal("-1.00"), new BigDecimal("-2.00")};
        SignedDecimalFieldOccursTwoTimes test2 = serializer.deserialize(serializer.serialize(test), SignedDecimalFieldOccursTwoTimes.class);
        assertArrayEquals(test.fields, test2.fields);
    }

}
