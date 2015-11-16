/*
 * Copyright (c) 2015. Troels Liebe Bentsen <tlb@nversion.dk>
 * Licensed under the MIT license (LICENSE.txt)
 */

package dk.nversion.copybook.serializers;


import java.nio.charset.Charset;
import java.util.List;

public class CopyBookSerializerConfig {
    private List<CopyBookField> fields;
    private Charset charset;
    private byte separatorByte;
    private int bitmapBlockSize;
    private boolean debug;

    public Charset getCharset() {
        return charset;
    }

    public void setCharset(Charset charset) {
        this.charset = charset;
    }

    public byte getSeparatorByte() {
        return separatorByte;
    }

    public void setSeparatorByte(byte separatorByte) {
        this.separatorByte = separatorByte;
    }

    public int getBitmapBlockSize() {
        return bitmapBlockSize;
    }

    public void setBitmapBlockSize(int bitmapBlockSize) {
        this.bitmapBlockSize = bitmapBlockSize;
    }

    public List<CopyBookField> getFields() {
        return fields;
    }

    public void setFields(List<CopyBookField> fields) {
        this.fields = fields;
    }

    public boolean isDebug() {
        return debug;
    }

    public void setDebug(boolean debug) {
        this.debug = debug;
    }
}
