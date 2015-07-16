package dk.nversion.copybook;

import java.lang.annotation.*;

@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
public @interface CopyBook {
    public CopyBookFormat format() default CopyBookFormat.FULL;
    public String charset() default "UTF-8";
}
