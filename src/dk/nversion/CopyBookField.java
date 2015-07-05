package dk.nversion;

import java.lang.annotation.*;

@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.FIELD)
@Repeatable(CopyBookFields.class)
public @interface CopyBookField {
    String value();
}

