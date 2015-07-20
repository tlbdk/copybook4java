package dk.nversion.copybook;

import java.lang.annotation.*;

@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
public @interface CopyBook {
    public CopyBookSerializationFormat format() default CopyBookSerializationFormat.NONE;
    public String charset() default "";
}
