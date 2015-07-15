package dk.nversion;

import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.util.Objects;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class CopyBookField {
    private Pattern re_pictype = Pattern.compile("(X|9|S9)\\((\\d+)\\)(?:V9\\((\\d+)\\))?");

    public CopyBookFieldType type;
    public int size;
    public int decimal;
    public Field[] fields;
    public int[] indexs;
    public int[] occurs;
    public String line;
    public CopyBookField counter;
    // TODO: Get this from the copybook comments
    public boolean rightpadding = true;
    public byte padding = (byte)32;

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

    public void setOccurs() {

    }

    public CopyBookField(CopyBookFieldType type, int size, int decimal, Field[] fields, int[] indexs) {
        this.type = type;
        this.size = size;
        this.decimal = decimal;
        this.fields = fields;
        this.indexs = indexs;
    }

    public Object get(Object obj) throws IllegalAccessException, CopyBookException {
        Object current = obj;
        for (int i = 0; i < fields.length; i++) {
            current = fields[i].get(current);
            // Handle array types
            if (current != null && indexs[i] > -1) {
                int current_length = Array.getLength(current);
                // Try to read the values in the array else return null
                if (indexs[i] < current_length) {
                    current = Array.get(current, indexs[i]);
                } else {
                    current = null;
                }

                // Validate that we can serialize this object
                if(current_length > occurs[i]) {
                    throw new CopyBookException("Array " + getFieldName(i) + " is larger than what is allowed by the copy book: " + occurs[i] + " < " + current_length);
                }
            }

            if (current == null) {
                break;
            }
        }
        return current;
    }

    public void set(Object obj, Object value, boolean recursive) throws IllegalAccessException, CopyBookException, InstantiationException {
        Object current = obj;
        for (int i = 0; i < fields.length - 1; i++) {
            // Get existing object
            Object nextcurrent = fields[i].get(current);

            // Create new object to hold value
            if(nextcurrent == null) {
                if(indexs[i] > -1) {
                    nextcurrent = Array.newInstance(fields[i].getType().getComponentType(), this.occurs[i]);
                } else {
                    nextcurrent = fields[i].getType().newInstance();
                }
                fields[i].set(current, nextcurrent);
            }

            if (indexs[i] > -1) {
                Object arrayitem = Array.get(nextcurrent, indexs[i]);
                if(arrayitem == null) {
                    arrayitem = fields[i].getType().getComponentType().newInstance();
                    Array.set(nextcurrent, indexs[i], arrayitem);
                }
                nextcurrent = arrayitem;
            }

            current = nextcurrent;
        }

        Field field = fields[fields.length -1];
        int index = indexs[indexs.length -1];
        if(index > -1) {
            Object nextcurrent = field.get(current);
            if(nextcurrent == null) {
                nextcurrent = Array.newInstance(fields[index].getType().getComponentType(), this.occurs[index]);
                field.set(current, nextcurrent);
            }
            Array.set(nextcurrent, index, value);

        } else {
            field.set(current, value);
        }
    }


    public String getFieldName() {
        return getFieldName(fields.length -1);
    }

    public String getFieldName(int index) {
        String result = this.fields[0].getName();
        for(int i=1; i <= index ; i++) {
            result += '.' + this.fields[i].getName();
        }
        return result;
    }

    public Field getLastField() {
        return fields[fields.length - 1];
    }

    public int getLastIndex() {
        return indexs[indexs.length -1];
    }

    public boolean isLastFieldArray() {
        return indexs[indexs.length -1] > -1;
    }

    public Object newInstanceLastField() throws IllegalAccessException, InstantiationException {
        return newInstance(fields.length - 1);
    }

    public Object newInstance(int index) throws IllegalAccessException, InstantiationException {
        if(isLastFieldArray()) {
            Object obj = Array.newInstance(this.fields[index].getType().getComponentType(), this.occurs[index]);
            for(int i=0; i < this.occurs[index]; i++) {
                Array.set(obj, i, this.fields[index].getType().getComponentType().newInstance());
            }
            return obj;

        } else {
            return this.fields[index].getType().newInstance();
        }

    }
}
