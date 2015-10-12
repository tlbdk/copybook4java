package dk.nversion.copybook.annotations;

import dk.nversion.copybook.CopyBookSerializationFormat;
import dk.nversion.copybook.serializers.CopyBookSerializerBase;

import java.lang.annotation.*;
import java.nio.charset.Charset;
import java.util.Optional;

@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
public @interface CopyBook {
    public Class<? extends CopyBookSerializerBase> type() default CopyBookSerializerBase.class; // Java sucks and we can use null as default value, so we pick something we would never user here
    public String charset() default "";
    // Java still sucks and does not support null as default value for annotations so we pick large G as this is å in EBCDIC and unlikely to be used as separator char
    public char separatorChar() default 'G';
    public int bitmapBlockSize() default 0;
}
