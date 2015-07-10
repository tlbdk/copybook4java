package dk.nversion;

import java.lang.reflect.Field;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class CopyBookField {
    private Pattern re_pictype = Pattern.compile("(X|9|S9)\\((\\d+)\\)(?:V9\\((\\d+)\\))?");

    public CopyBookType type;
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
                this.type = CopyBookType.STRING;
                this.size = Integer.parseInt(matcher.group(2));
                this.decimal = -1;

            } else if (matcher.group(1).equals("S9")) { // Signed number
                if(matcher.group(3) != null) {
                    // With decimals
                    this.type = CopyBookType.SIGNED_INT;
                    this.size = Integer.parseInt(matcher.group(2));
                    this.decimal = Integer.parseInt(matcher.group(3));

                } else {
                    // Without decimals
                    this.type = CopyBookType.DECIMAL;
                    this.size = Integer.parseInt(matcher.group(2));
                    this.decimal = Integer.parseInt(matcher.group(3));
                }

            } else if (matcher.group(1).equals("9")) { // unsigned number
                // Check if it's a decimal number
                if(matcher.group(3) != null) {
                    // With decimals
                    this.type = CopyBookType.DECIMAL;
                    this.size = Integer.parseInt(matcher.group(2));
                    this.decimal = -1;

                } else {
                    // Without decimals
                    this.type = CopyBookType.INT;
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

    public CopyBookField(CopyBookType type, int size, int decimal, Field[] fields, int[] indexs) {
        this.type = type;
        this.size = size;
        this.decimal = decimal;
        this.fields = fields;
        this.indexs = indexs;
    }
}
