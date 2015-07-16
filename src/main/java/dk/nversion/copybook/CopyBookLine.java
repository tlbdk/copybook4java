package dk.nversion.copybook;

import java.lang.annotation.*;

@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.FIELD)
@Repeatable(CopyBookLines.class)
public @interface CopyBookLine {
    String value();
}

