package dk.nversion.copybook;

import java.lang.annotation.*;
import java.nio.charset.Charset;
import java.util.Optional;

@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
public @interface CopyBook {
    public CopyBookSerializationFormat format() default CopyBookSerializationFormat.NONE;
    public String charset() default "";
    // Java sucks and does not support null as default value for annotations so we pick large G as this is å in EBCDIC and unlikely to be used as separator char
    public char separatorChar() default 'G';
    public int bitmapBlockSize() default 0;
}
