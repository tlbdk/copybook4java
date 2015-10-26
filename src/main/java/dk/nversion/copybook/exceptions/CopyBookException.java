package dk.nversion.copybook.exceptions;

public class CopyBookException extends Exception {
    public CopyBookException(String message) {
        super(message);
    }

    public CopyBookException(String message, Exception ex) {
        super(message + ": " + ex.getClass().getSimpleName() + " :" + ex.getMessage());
    }

}
