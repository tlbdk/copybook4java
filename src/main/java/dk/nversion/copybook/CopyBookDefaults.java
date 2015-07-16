package dk.nversion.copybook;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@CopyBook(format = CopyBookFormat.FULL, charset = "UTF-8")
@CopyBookPadding(fieldType = CopyBookFieldType.INT, right = false, character  = '0')
@CopyBookPadding(fieldType = CopyBookFieldType.SIGNED_INT, right = false, character  = '0')
@CopyBookPadding(fieldType = CopyBookFieldType.DECIMAL, right = false, character  = '0')
@CopyBookPadding(fieldType = CopyBookFieldType.SIGNED_DECIMAL, right = false, character  = '0')
@CopyBookPadding(fieldType = CopyBookFieldType.STRING, right = true, character  = ' ')
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
public @interface CopyBookDefaults {
}
