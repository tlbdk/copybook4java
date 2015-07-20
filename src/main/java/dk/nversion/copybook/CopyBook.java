package dk.nversion.copybook;

import java.lang.annotation.*;

@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
public @interface CopyBook {
    public CopyBookSerializationFormat format() default CopyBookSerializationFormat.FULL;
    public String charset() default "UTF-8";
}
