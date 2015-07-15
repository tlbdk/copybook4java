package dk.nversion;

import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.nio.ByteBuffer;
import java.nio.charset.Charset;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;


// http://www-01.ibm.com/support/knowledgecenter/SSXJAV_13.1.0/com.ibm.filemanager.doc_13.1/db2/fmnu2113.htm

public class CopyBookSerializer {
    private Pattern re_occurs = Pattern.compile("OCCURS\\s+(\\d+)\\s+TIMES");
    private List<CopyBookField> cbfields = new ArrayList<CopyBookField>();
    private int recordsize = 0;
    private CopyBookFormat format;
    private Charset charset;

    public CopyBookSerializer() {

    }

    public <T> CopyBookSerializer(Class<T> type, CopyBookFormat format, Charset charset) throws Exception {
        this.format = format;
        this.charset = charset;

        this.cbfields = walkClass(type, new Field[0], new int[0], new int[0]);
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
            System.out.print("], ");
            System.out.print(", [");
            for(int index : cbfield.occurs) {
                System.out.print(index + ",");
            }
            System.out.print("], ");
            System.out.println();
        }
    }

    // Walk and find all copybook annotations and flatten to a list of CopyBookfields
    private <T> List<CopyBookField> walkClass(Class<T> type, Field[] fields, int[] indexes, int[] occurs) throws Exception {
        List<CopyBookField> results = new ArrayList<>();
        Map<String, CopyBookField> fieldnames = new HashMap<>();

        //TODO: Validate that copybook matches the fields

        // Itegrate over the class fields with CopyBookLine annotation
        for (Field field : type.getDeclaredFields()) {
            Class fieldclass = field.getType();
            CopyBookLine[] cbls = (CopyBookLine[])field.getAnnotationsByType(CopyBookLine.class);

            // Handle private fields
            if(!field.isAccessible()) {
                field.setAccessible(true);
            }

            // Append new field and index to arrays
            Field[] currentfields =arrayAppend(fields, field);

            if(cbls.length == 0) {
                // No CopyBookLine on this field

            } else if(cbls.length == 1) {
                System.out.println(new String(new char[currentfields.length * 2]).replace("\0", " ") + cbls[0].value());
                int occurscount = getOccurs(cbls[0].value());

                if(occurscount > 1) {
                    if(fieldclass.isArray() && fieldclass.getComponentType().getPackage() == type.getPackage()) {
                        // Array type in package
                        for (int i=0; i < occurscount; i++) {
                            results.addAll(walkClass(fieldclass.getComponentType(), currentfields, arrayAppend(indexes, i), arrayAppend(occurs, occurscount)));
                        }
                    }

                } else if(fieldclass.getPackage() == type.getPackage()) {
                    // Complex type in package
                    results.addAll(walkClass(fieldclass, currentfields, arrayAppend(indexes, -1), arrayAppend(occurs, occurscount)));

                } else {
                    // Simple types, such as int and String
                    CopyBookField cbf = new CopyBookField(cbls[0].value());
                    cbf.fields = currentfields;
                    cbf.indexs = arrayAppend(indexes, -1);
                    cbf.occurs = arrayAppend(occurs, occurscount);
                    results.add(cbf);
                    fieldnames.put(field.getName(), cbf);

                    // Reference counter field if exists
                    String name = field.getName();
                    if(name.endsWith("_count")) {
                        CopyBookField refcbf = fieldnames.get(name.substring(name.length() - 6));
                        if(refcbf != null) {
                            refcbf.counter = cbf;
                        }
                    } else {
                        CopyBookField refcbf = fieldnames.get(name + "_count");
                        if(refcbf != null) {
                            cbf.counter = refcbf;
                        }
                    }
                }

            } else if(cbls.length == 2) {
                System.out.println(new String(new char[currentfields.length * 2]).replace("\0", " ") + cbls[0].value());
                int occurscount = getOccurs(cbls[0].value());
                if(occurscount > 1) {
                    // Simple array types, such as int[] and String[]
                    for (int i = 0; i < occurscount; i++) {
                        System.out.println(new String(new char[currentfields.length * 2 + 2]).replace("\0", " ") + cbls[1].value());
                        CopyBookField cbf = new CopyBookField(cbls[1].value());
                        cbf.fields = currentfields;
                        cbf.indexs = arrayAppend(indexes, i);
                        cbf.occurs = arrayAppend(occurs, occurscount);
                        results.add(cbf);
                        fieldnames.put(field.getName(), cbf);

                        // Reference counter field if exists
                        String name = field.getName();
                        if(name.endsWith("_count")) {
                            CopyBookField refcbf = fieldnames.get(name.substring(name.length() - 6));
                            if(refcbf != null) {
                                refcbf.counter = cbf;
                            }
                        } else {
                            CopyBookField refcbf = fieldnames.get(name + "_count");
                            if(refcbf != null) {
                                cbf.counter = refcbf;
                            }
                        }
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
            Object current = cbfield.get(obj);
            if (current != null) {
                byte[] result;
                byte[] bytes;
                switch (cbfield.type) {
                    case STRING: {
                        bytes = ((String)current).getBytes(charset);
                        break;
                    }
                    case SIGNED_INT:
                    case INT: {
                        bytes = current.toString().getBytes(charset);
                        break;
                    }
                    case SIGNED_DECIMAL:
                    case DECIMAL: {
                        bytes = current.toString().getBytes(charset);
                        break;
                    }
                    default: {
                        throw new CopyBookException("Unknown copybook field type");
                    }
                }

                if(bytes.length <= cbfield.size) {
                    result = new byte[cbfield.size];
                    Arrays.fill(result, cbfield.padding); // TODO: Get from field
                    if(cbfield.rightpadding) {
                        System.arraycopy(bytes, 0, result, 0, bytes.length);
                    } else {
                        System.arraycopy(bytes, 0, result, result.length - bytes.length, bytes.length);
                    }

                } else {
                    throw new CopyBookException("Field '"+ cbfield.getFieldName() +"' to long : " + bytes.length + " > " + cbfield.size);
                }

                buf.put(result); // Add space as padding
                System.out.println(cbfield.type + "(" + cbfield.size + ","+ result.length +"): " + current.toString());

            } else {
                // Write empty space for missing obj
                byte[] filler = new byte[cbfield.size];
                Arrays.fill(filler, (byte)32);
                buf.put(filler);
                System.out.println(cbfield.type.name() + "(" + cbfield.size + "): " + "______");
            }
        }

        return buf.array();
    }

    private <T> T[] arrayAppend(T[] array, T obj) {
        T[] newarray = Arrays.copyOf(array, array.length + 1);
        newarray[newarray.length -1] = obj;
        return newarray;
    }

    private int[] arrayAppend(int[] array, int obj) {
        int[] newarray = Arrays.copyOf(array, array.length + 1);
        newarray[newarray.length -1] = obj;
        return newarray;
    }

    private int getOccurs(String str) {
        Matcher matcher = re_occurs.matcher(str);
        if(matcher.find()) {
            return Integer.parseInt(matcher.group(1));
        } else {
            return -1;
        }
    }

    public <T> T deserialize(byte[] data, Class<T> type) throws CopyBookException, InstantiationException {
        if(this.format == CopyBookFormat.FULL) {
            return deserializeFull(data, type);

        } else if (this.format == CopyBookFormat.PACKED) {
            return deserializePacked(data, type);

        } else {
            throw new CopyBookException("Unsupported format");
        }
    }

    private <T> T deserializeFull(byte[] data, Class<T> type) throws CopyBookException, InstantiationException {
        try {
            T obj = type.newInstance();
            ByteBuffer buf = ByteBuffer.wrap(data);
            for(CopyBookField cbfield : cbfields) {
                // Convert field bytes to string and trim value
                byte[] bytevalue =  new byte[cbfield.size];
                buf.get(bytevalue);
                String strvalue = new String(bytevalue, charset);
                strvalue = strvalue.trim(); //TODO: remove padding chars as this might not be space

                // Convert to native types
                try {
                    Object result;
                    Class fieldtype = cbfield.getLastField().getType();
                    switch (cbfield.type) {
                        case STRING: {
                            result = strvalue;
                            break;
                        }
                        case SIGNED_INT:
                        case INT: {
                            if(fieldtype.equals(Integer.TYPE)) {
                                result = Integer.parseInt(strvalue);
                            } else if (fieldtype.equals(Long.TYPE)) {
                                result = Long.parseLong(strvalue);
                            } else {
                                throw new CopyBookException("Field did not match type : " + cbfield.getFieldName());
                            }
                            break;
                        }
                        case SIGNED_DECIMAL:
                        case DECIMAL: {
                            if(fieldtype.equals(Float.TYPE)) {
                                result = Float.parseFloat(strvalue);
                            } else if (fieldtype.equals(Double.TYPE)) {
                                result = Double.parseDouble(strvalue);
                            } else {
                                throw new CopyBookException("Field did not match type : " + cbfield.getFieldName());
                            }
                            break;
                        }
                        default: {
                            throw new CopyBookException("Unknown copybook field type");
                        }
                    }

                    // TODO: Skip set if we have read the counter field and found it to be outside the limits
                    cbfield.set(obj, result, true);

                } catch (NumberFormatException ex) {
                    // TODO: Error with field that failed validation
                }
            }

            return obj;

        } catch(IllegalAccessException ex) {
            throw new CopyBookException("Failed to deserialize bytes"); // TODO: Add more information on why we failed
        }
    }

    private <T> T deserializePacked(byte[] data, Class<T> type) throws CopyBookException {
        return null;
    }
}