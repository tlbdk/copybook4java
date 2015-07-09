package dk.nversion;

import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;


// http://www-01.ibm.com/support/knowledgecenter/SSXJAV_13.1.0/com.ibm.filemanager.doc_13.1/db2/fmnu2113.htm

public class CopyBookSerializer {
    private Pattern re_occurs = Pattern.compile("OCCURS\\s+(\\d+)\\s+TIMES");
    private List<CopyBookField> cbfields = new ArrayList<CopyBookField>();
    private int recordsize = 0;

    public CopyBookSerializer() {

    }

    public <T> CopyBookSerializer(Class<T> type) throws Exception {
        cbfields = walkClass(type, new Field[0], new int[0]);
        for(CopyBookField cbfield : cbfields) {
            recordsize += cbfield.size;

            System.out.print("[");
            for(Field field : cbfield.fields) {
                System.out.print(field.getName() + ",");
            }
            System.out.print("]");
            System.out.print(", ");
            System.out.print(cbfield.size);
            System.out.print(", [");
            for(int index : cbfield.indexs) {
                System.out.print(index + ",");
            }
            System.out.print("]");
            System.out.println();
        }
    }

    // Walk and find all copybook annotations and flatten to a list of CopyBookfields
    private <T> List<CopyBookField> walkClass(Class<T> type, Field[] fields, int[] indexes) throws Exception {
        List<CopyBookField> results = new ArrayList<>();
        //TODO: Validate that copybook matches the fields

        // Itegrate over the class fields with CopyBookLine annotation
        for (Field field : type.getDeclaredFields()) {
            Class fieldclass = field.getType();
            CopyBookLine[] cbls = (CopyBookLine[])field.getAnnotationsByType(CopyBookLine.class);

            // Append new field and index to arrays
            Field[] currentfields = Arrays.copyOf(fields, fields.length + 1);
            currentfields[currentfields.length -1] = field;

            if(cbls.length == 0) {
                // No CopyBookLine on this field

            } else if(cbls.length == 1) {
                System.out.println(new String(new char[currentfields.length * 2]).replace("\0", " ") + cbls[0].value());
                int occurs = getOccurs(cbls[0].value());

                if(occurs > 1) {
                    if(fieldclass.isArray() && fieldclass.getComponentType().getPackage() == type.getPackage()) {
                        // Array type in package
                        for (int i=0; i < occurs; i++) {
                            int[] currentindexes = Arrays.copyOf(indexes, indexes.length + 1);
                            currentindexes[currentindexes.length -1] = i;
                            results.addAll(walkClass(fieldclass.getComponentType(), currentfields, currentindexes));
                        }
                    }

                } else if(fieldclass.getPackage() == type.getPackage()) {
                    // Complex type in package
                    int[] currentindexes = Arrays.copyOf(indexes, indexes.length + 1);
                    currentindexes[currentindexes.length -1] = -1;
                    results.addAll(walkClass(fieldclass, currentfields, currentindexes));

                } else {
                    int[] currentindexes = Arrays.copyOf(indexes, indexes.length + 1);
                    currentindexes[currentindexes.length -1] = -1;

                    // Simple types, such as int and String
                    CopyBookField cbf = new CopyBookField(cbls[0].value());
                    cbf.fields = currentfields;
                    cbf.indexs = currentindexes;
                    results.add(cbf);
                }

            } else if(cbls.length == 2) {
                System.out.println(new String(new char[currentfields.length * 2]).replace("\0", " ") +  cbls[0].value());
                int occurs = getOccurs(cbls[0].value());
                if(occurs > 1) {
                    // Simple array types, such as int[] and String[]
                    for (int i = 0; i < occurs; i++) {
                        System.out.println(new String(new char[currentfields.length * 2 + 2]).replace("\0", " ") + cbls[1].value());
                        CopyBookField cbf = new CopyBookField(cbls[1].value());
                        cbf.fields = currentfields;
                        int[] currentindexes = Arrays.copyOf(indexes, indexes.length + 1);
                        currentindexes[currentindexes.length -1] = i;
                        cbf.indexs = currentindexes;
                        results.add(cbf);
                    }

                } else {
                    throw new Exception("Field is missing CopyBookLine with OCCURS");
                }
            }
        }

        return results;
    }

    /*

[0] : [id], 8, [-1]
[1] : [args], 8, [-1]
[1] : [args], 8, [-1]
[2] : [message, title], 20, [-1]
[3] : [message, body], 200, [-1]
[4] : [message, comments], 200, [-1, 0]
[5] : [message, comments], 200, [-1, 1]
[6] : [messages, title], 20, [0, -1]
[7] : [messages, body], 200, [0, -1]
[8] : [messages, comments], 200, [0, 0]
[9] : [messages, comments], 200, [0, 1]
[10] : [messages, title], 20, [1, -1]
[11] : [messages, body], 200, [1, -1]
[12] : [messages, comments], 200, [1, 0]
[13] : [messages, comments], 200, [1, 1]
     */


    public <T> byte[] serialize(T obj) throws Exception {
        ByteBuffer buf = ByteBuffer.wrap(new byte[this.recordsize]);
        for(CopyBookField cbfield : cbfields) {

            // Resolve obj for each fields
            Object current = obj;
            for (int i = 0; i < cbfield.fields.length; i++) {
                current = cbfield.fields[i].get(current);
                // Handle array types
                if (current != null && cbfield.indexs[i] > -1) {
                    if (cbfield.indexs[i] < Array.getLength(current)) {
                        // Index smaller than actual array size
                        current = Array.get(current, cbfield.indexs[i]);
                    } else {
                        current = null;
                    }
                }

                if (current == null) {
                    break;
                }
            }

            if (current != null) {
                // Write cobol bytes to stream
                writeCobolBytes(buf, current, cbfield.size, cbfield.type);
                System.out.println(cbfield.type + "(" + cbfield.size + "): " + current.toString());


            } else {
                // Write empty space for missing obj
                buf.put(new byte[cbfield.size]);
                System.out.println(cbfield.type.name() + "(" + cbfield.size + "): " + "______");
            }
        }

        return buf.array();
    }

    private void writeCobolBytes(ByteBuffer buf, Object obj, int size, CopyBookType type) throws Exception {

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