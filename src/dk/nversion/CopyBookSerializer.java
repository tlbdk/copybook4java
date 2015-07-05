package dk.nversion;

import java.lang.annotation.Annotation;
import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;


// http://www-01.ibm.com/support/knowledgecenter/SSXJAV_13.1.0/com.ibm.filemanager.doc_13.1/db2/fmnu2113.htm
public class CopyBookSerializer {
    Pattern re_occurs = Pattern.compile("OCCURS\\s+(\\d+)\\s+TIMES");
    Pattern re_pictype = Pattern.compile("(X|9|S9)\\((\\d+)\\)(?:V9\\((\\d+)\\))?");

    public CopyBookSerializer() {

    }

    public <T> CopyBookSerializer(Class<T> type) {
        //TODO: Walk and find all copybook annotations and calc size of each secion
    }

    public <T> void serialize(T obj, byte[] buf, int offset) throws Exception {
        // Itegrate over the class fields with CopyBookField annotation
        for (Field field : obj.getClass().getDeclaredFields()) {
            CopyBookField[] cbfs = (CopyBookField[])field.getAnnotationsByType(CopyBookField.class);
            if(cbfs.length == 0) {
                // No CopyBookField on this field
            
            } else if(cbfs.length == 1) {
                System.out.println("::" + cbfs[0].value());
                int occurs = getOccurs(cbfs[0].value());
                if(occurs > 1) {
                    int length = Array.getLength(field.get(obj));
                    for (int i = 0; i < length; i ++) {
                        serialize(Array.get(field.get(obj), i), buf, offset);
                    }

                } else if(obj.getClass().getPackage() == field.get(obj).getClass().getPackage()) {
                    serialize(field.get(obj), buf, offset);

                } else {
                    byte[] bytes = getCobolBytes(field.get(obj), cbfs[0].value());
                    System.out.println(new String(bytes));
                    //TODO: Move offset by: bytes.length
                }

            } else if(cbfs.length == 2) {
                System.out.println("::" + cbfs[0].value());
                int occurs = getOccurs(cbfs[0].value());
                if(occurs > 1) {
                    int length = Array.getLength(field.get(obj));
                    for (int i = 0; i < length; i++) {
                        System.out.println("::" + cbfs[1].value());
                        byte[] bytes = getCobolBytes(Array.get(field.get(obj), i), cbfs[1].value());
                        System.out.println(new String(bytes));
                    }
                    //TODO: Move offset by: bytes.length * occurs , also handle if length is 0
                } else {
                    throw new Exception("Field is missing CopyBookField with OCCURS");
                }
            }
        }


        /*List<Field> fields = new ArrayList<Field>();
        Deque<Class> classes = new ArrayDeque<Class>();
        classes.add(obj.getClass());

        Class current;
        int recordsize = 0;
        while(!classes.isEmpty()) {
            current = classes.pop();
            System.out.println("class:" + current.getCanonicalName());
            for (Field field : current.getDeclaredFields()) {
                Class c = field.getType();
                CopyBookField cbf = field.getAnnotation(CopyBookField.class);
                if(cbf != null) {
                    // Is this an static inner class of the object then we need to recurse to see if we have more annotations
                    if(c.isArray() && c.getComponentType().getPackage() == current.getPackage()) {
                        classes.add(c.getComponentType());
                    } else if(c.getPackage() == current.getPackage()) {
                        classes.add(c);
                    } else {
                        recordsize += cbf.size() * cbf.occurs();
                        fields.add(field);
                    }
                    System.out.println("field:" + field);
                }
            }
        }
        System.out.println("size:" + recordsize); */

    }

    private byte[] getCobolBytes(Object obj, String picstr) throws Exception {
        Matcher matcher = re_pictype.matcher(picstr);
        if(matcher.find()) {
            // String type
            if(matcher.group(1).equals("X")) {
                byte[] buffer = new byte[Integer.parseInt(matcher.group(2))];
                byte[] result = obj.toString().getBytes(); // TODO: Fix charset
                System.arraycopy(result, 0, buffer, 0,result.length);
                // TODO: Do padding, fx. write zero in empty part
                return result;

            } else if (matcher.group(1).equals("S9")) {
                if(matcher.group(3) != null) {
                    // Signed decimal number
                    byte[] buffer = new byte[Integer.parseInt(matcher.group(2))];
                    byte[] result = obj.toString().getBytes(); // TODO: Fix charset
                    System.arraycopy(result, 0, buffer, 0,result.length);
                    // TODO: Do padding, fx. write zero in empty part
                    return result;

                } else {
                    // Signed integer
                    byte[] buffer = new byte[Integer.parseInt(matcher.group(2))];
                    byte[] result = obj.toString().getBytes(); // TODO: Fix charset
                    System.arraycopy(result, 0, buffer, 0,result.length);
                    // TODO: Do padding, fx. write zero in empty part
                    return result;
                }

            } else if (matcher.group(1).equals("9")) {
                // Check if it's a decimal number
                if(matcher.group(3) != null) {
                    // Unsigned decimal
                    byte[] buffer = new byte[Integer.parseInt(matcher.group(2))];
                    byte[] result = obj.toString().getBytes(); // TODO: Fix charset
                    System.arraycopy(result, 0, buffer, 0,result.length);
                    // TODO: Do padding, fx. write zero in empty part
                    return result;
                } else {
                    // Unsigned integer
                    byte[] buffer = new byte[Integer.parseInt(matcher.group(2))];
                    byte[] result = obj.toString().getBytes(); // TODO: Fix charset
                    System.arraycopy(result, 0, buffer, 0,result.length);
                    // TODO: Do padding, fx. write zero in empty part
                    return result;
                }
            } else {
                throw new Exception("Unknown PIC type");
            }

        } else {
            throw new Exception("Could not find any PIC type");
        }
    }


    private int getOccurs(String str) {
        Matcher matcher = re_occurs.matcher(str);
        if(matcher.find()) {
            return Integer.parseInt(matcher.group(1));
        } else {
            return -1;
        }
    }

    public <T> T deserialize(byte[] data, Class<T> type) {
        return null;
    }

}