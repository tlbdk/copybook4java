package dk.nversion;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Arrays;

public class ByteUtils {

    public static boolean allEquals(byte[] array, byte value, int offset, int maxLength) {
        maxLength = Math.min(offset + maxLength, array.length);
        for(int i = offset; i < maxLength; i++) {
            if(array[i] != value) {
                return false;
            }
        }
        return true;
    }

    public static int indexOf(byte[] array, byte needle, int offset, int maxLength) {
        int result = -1;
        maxLength = Math.min(offset + maxLength, array.length);
        for(int i = offset; i < maxLength; i++) {
            if(array[i] == needle) {
                result = i;
                break;
            }
        }
        return result;
    }

    public static byte[] trim(byte[] src, byte padding, boolean right, int minLength) {
        if(src.length < minLength) {
            throw new RuntimeException("src array is smaller than minLength: " + src.length + " < " + minLength);
        }
        if(right) {
            int offset;
            for (offset = src.length - 1; offset > minLength - 1; offset--) {  // [44, 32]
                if(padding != src[offset]) {
                    break;
                }
            }
            if(offset < 0) {
                return new byte[0];
            } else if (offset < src.length - 1){
                return Arrays.copyOfRange(src, 0, offset + 1);
            }

        } else {
            int offset;
            for (offset = 0; offset < src.length - minLength; offset++) {
                if(padding != src[offset]) {
                    break;
                }
            }
            if(offset == src.length) {
                return new byte[0];
            } else if(offset > 0) {
                return Arrays.copyOfRange(src, offset, src.length);
            }
        }

        return src;
    }

    public static byte[] toByteArray(InputStream inputStream) throws IOException {
        ByteArrayOutputStream outputStream = new ByteArrayOutputStream();

        byte[] buf = new byte[8192];
        while(true) {
            int r = inputStream.read(buf);
            if(r == -1) {
                break;
            }
            outputStream.write(buf, 0, r);
        }

        return outputStream.toByteArray();
    }
}
