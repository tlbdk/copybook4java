package dk.nversion;

import java.util.Arrays;

public class ByteUtils {

    public static int indexOf(byte[] array, byte needle, int offset, int length) {
        int result = -1;
        length += offset;
        for(int i = offset; i < length; i++) {
            if(array[i] == needle) {
                result = i;
            }
        }
        return result;
    }

    public static byte[] trim(byte[] src, byte padding, boolean right, int minLength) {
        if(right) {
            int offset;
            for (offset = src.length - 1; offset > minLength; offset--) {
                if(padding != src[offset]) {
                    break;
                }
            }
            if(offset == src.length) {
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
}
