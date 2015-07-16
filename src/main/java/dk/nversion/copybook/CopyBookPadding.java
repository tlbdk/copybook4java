package dk.nversion.copybook;

import java.lang.annotation.*;

@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE, ElementType.FIELD })
@Repeatable(CopyBookPaddings.class)
public @interface CopyBookPadding {
    public CopyBookFieldType fieldType();
    public boolean right();
    public char character();
}
