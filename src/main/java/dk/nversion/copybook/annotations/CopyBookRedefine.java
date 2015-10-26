package dk.nversion.copybook.annotations;

import java.lang.annotation.*;

@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
@Repeatable(CopyBookRedefines.class)
public @interface CopyBookRedefine {
    Class value();
    String match() default "";
}
