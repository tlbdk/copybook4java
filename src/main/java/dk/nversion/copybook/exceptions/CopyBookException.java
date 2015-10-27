package dk.nversion.copybook.exceptions;

import dk.nversion.copybook.converters.TypeConverterException;

public class CopyBookException extends Exception {
    public CopyBookException(String message) {
        super(message);
    }

    public CopyBookException(String message, TypeConverterException ex) {
        super(message + ": " + ex.getClass().getSimpleName() + " :" + ex.getMessage());
    }

    public CopyBookException(String message, Exception ex) {
        super(message + ": " + ex.getClass().getSimpleName() + " :" + ex.getMessage());
    }
}
