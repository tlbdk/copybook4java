package dk.nversion;

import java.lang.reflect.Field;
import java.nio.charset.Charset;
import java.util.Arrays;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class CopyBookField {
    private Pattern re_pictype = Pattern.compile("(X|9|S9)\\((\\d+)\\)(?:V9\\((\\d+)\\))?");

    public CopyBookFieldType type;
    public int size;
    public int decimal;
    public Field[] fields;
    public int[] indexs;
    public String line;
    public Field counter;

    public CopyBookField() {

    }

    public CopyBookField(String copybookline) throws Exception {
        this.line = copybookline;
        Matcher matcher = re_pictype.matcher(copybookline);
        if(matcher.find()) {
            if(matcher.group(1).equals("X")) { // String type
                // String
                this.type = CopyBookFieldType.STRING;
                this.size = Integer.parseInt(matcher.group(2));
                this.decimal = -1;

            } else if (matcher.group(1).equals("S9")) { // Signed number
                if(matcher.group(3) != null) {
                    // With decimals
                    this.type = CopyBookFieldType.SIGNED_INT;
                    this.size = Integer.parseInt(matcher.group(2));
                    this.decimal = Integer.parseInt(matcher.group(3));

                } else {
                    // Without decimals
                    this.type = CopyBookFieldType.DECIMAL;
                    this.size = Integer.parseInt(matcher.group(2));
                    this.decimal = Integer.parseInt(matcher.group(3));
                }

            } else if (matcher.group(1).equals("9")) { // unsigned number
                // Check if it's a decimal number
                if(matcher.group(3) != null) {
                    // With decimals
                    this.type = CopyBookFieldType.DECIMAL;
                    this.size = Integer.parseInt(matcher.group(2));
                    this.decimal = -1;

                } else {
                    // Without decimals
                    this.type = CopyBookFieldType.INT;
                    this.size = Integer.parseInt(matcher.group(2));
                    this.decimal = -1;
                }
            } else {
                throw new Exception("Unknown PIC type");
            }

        } else {
            throw new Exception("Could not find any PIC type");
        }
    }

    public byte[] getBytes(Object obj, CopyBookOutputFormat format, Charset charset, byte padding, boolean leftalign) throws CopyBookException {
        byte[] bytes;
        switch (this.type) {
            case STRING: {
                bytes = ((String)obj).getBytes(charset);
                break;
            }
            case SIGNED_INT: {
                bytes = obj.toString().getBytes(charset);
                break;
            }
            case INT: {
                bytes = obj.toString().getBytes(charset);
                break;
            }
            case SIGNED_DECIMAL: {
                bytes = obj.toString().getBytes(charset);
                break;
            }
            case DECIMAL: {
                bytes = obj.toString().getBytes(charset);
                break;
            }
            default: {
                throw new CopyBookException("Unknown copybook field type");
            }
        }

        if(bytes.length <= this.size) {
            byte[] result = new byte[this.size];
            Arrays.fill(bytes, padding);
            if(leftalign) {
                System.arraycopy(bytes, 0, result, 0, bytes.length);
            } else {
                System.arraycopy(bytes, 0, result, result.length - bytes.length, bytes.length);
            }
            return result;

        } else {
            throw new CopyBookException("Field '"+ this.getFieldName() +"' to long : " + bytes.length + " > " + this.size);
        }
    }

    public CopyBookField(CopyBookFieldType type, int size, int decimal, Field[] fields, int[] indexs) {
        this.type = type;
        this.size = size;
        this.decimal = decimal;
        this.fields = fields;
        this.indexs = indexs;
    }

    public String getFieldName() {
        String result = this.fields[0].getName();
        for(int i=1; i < this.fields.length - 1; i++) {
            result += '.' + this.fields[i].getName();
        }
        return result;
    }
}
