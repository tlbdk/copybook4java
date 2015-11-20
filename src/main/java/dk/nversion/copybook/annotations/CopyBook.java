/*
 * Copyright (c) 2015. Troels Liebe Bentsen <tlb@nversion.dk>
 * Licensed under the MIT license (LICENSE.txt)
 */

package dk.nversion.copybook.annotations;

import dk.nversion.copybook.serializers.CopyBookMapper;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
public @interface CopyBook {
    Class<? extends CopyBookMapper> type() default CopyBookMapper.class; // Java sucks and we can use null as default value, so we pick something we would never user here
    String charset() default "";
    // Java still sucks and does not support null as default value for annotations so we pick large G as this is å in EBCDIC and unlikely to be used as separator char
    char separatorChar() default 'G';
    int bitmapBlockSize() default 0;
    String counterFormat() default ""; // TODO: Implement "X{fieldName}"
}
