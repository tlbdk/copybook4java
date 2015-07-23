package dk.nversion.copybook;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@CopyBook(format = CopyBookSerializationFormat.FULL, charset = "UTF-8")
@CopyBookFieldFormat(fieldType = CopyBookFieldType.INT, rightPadding = false, paddingChar  = '0', signingPostfix = false)
@CopyBookFieldFormat(fieldType = CopyBookFieldType.SIGNED_INT, rightPadding = false, paddingChar  = '0', signingPostfix = false)
@CopyBookFieldFormat(fieldType = CopyBookFieldType.DECIMAL, rightPadding = false, paddingChar  = '0', signingPostfix = false )
@CopyBookFieldFormat(fieldType = CopyBookFieldType.SIGNED_DECIMAL, rightPadding = false, paddingChar  = '0', signingPostfix = false)
@CopyBookFieldFormat(fieldType = CopyBookFieldType.STRING, rightPadding = true, paddingChar  = ' ',  signingPostfix = false)
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
public @interface CopyBookDefaults {
}
