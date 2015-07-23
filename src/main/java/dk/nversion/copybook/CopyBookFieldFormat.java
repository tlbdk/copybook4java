package dk.nversion.copybook;

import java.lang.annotation.*;

@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE, ElementType.FIELD })
@Repeatable(CopyBookFieldFormats.class)
public @interface CopyBookFieldFormat {
    public CopyBookFieldType fieldType();
    public boolean rightPadding();
    public char paddingChar();
    public char nullFillerChar();
    public boolean signingPostfix();
}
