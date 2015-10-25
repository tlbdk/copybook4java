package dk.nversion.copybook.annotations;

import dk.nversion.copybook.converters.TypeConverter;
import dk.nversion.copybook.serializers.CopyBookFieldSigningType;
import java.lang.annotation.*;

@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE, ElementType.FIELD })
@Repeatable(CopyBookFieldFormats.class)
public @interface CopyBookFieldFormat {
    Class<? extends TypeConverter> type();

    CopyBookFieldSigningType signingType() default CopyBookFieldSigningType.PREFIX;
    boolean rightPadding() default false;
    char paddingChar() default ' ';
    char nullFillerChar() default (byte)0;
    String defaultValue() default "";
    // String format() default ""; // TODO: Implement
}
