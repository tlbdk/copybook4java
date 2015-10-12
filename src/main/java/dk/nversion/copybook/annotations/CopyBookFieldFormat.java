package dk.nversion.copybook.annotations;

import dk.nversion.copybook.CopyBookFieldSigningType;
import dk.nversion.copybook.CopyBookFieldType;
import dk.nversion.copybook.converters.TypeConverterBase;

import java.lang.annotation.*;

@Retention(RetentionPolicy.RUNTIME)
@Target({ ElementType.TYPE, ElementType.FIELD })
@Repeatable(CopyBookFieldFormats.class)
public @interface CopyBookFieldFormat {
    Class<? extends TypeConverterBase> type();
    CopyBookFieldSigningType signingType();

    boolean rightPadding();
    char paddingChar();
    char nullFillerChar();

}
