/*
 * Copyright (c) 2015. Troels Liebe Bentsen <tlb@nversion.dk>
 * Licensed under the MIT license (LICENSE.txt)
 */

package dk.nversion.copybook.exceptions;

public class TypeConverterException extends RuntimeException {
    private static final long serialVersionUID = -7577535992947276304L;
    public TypeConverterException(String message) {
        super(message);
    }
}
