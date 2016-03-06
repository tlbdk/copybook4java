/*
 * Copyright (c) 2015. Troels Liebe Bentsen <tlb@nversion.dk>
 * Licensed under the MIT license (LICENSE.txt)
 */

package dk.nversion.copybook.serializers;

public enum CopyBookFieldSigningType {
    POSTFIX,
    PREFIX,
    LAST_BYTE_BIT8,
    LAST_BYTE_EBCDIC_BIT5
}