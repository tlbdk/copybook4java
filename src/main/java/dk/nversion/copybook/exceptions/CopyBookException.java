package dk.nversion.copybook.exceptions;

import dk.nversion.copybook.converters.TypeConverterException;

// TODO: use RunTimeException
public class CopyBookException extends Exception {
    private static final long serialVersionUID = 28118369047109260L;
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
