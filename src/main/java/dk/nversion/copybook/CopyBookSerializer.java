package dk.nversion.copybook;

import dk.nversion.ByteUtils;

import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.nio.ByteBuffer;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static java.util.stream.Collectors.joining;


// http://www-01.ibm.com/support/knowledgecenter/SSXJAV_13.1.0/com.ibm.filemanager.doc_13.1/db2/fmnu2113.htm

public class CopyBookSerializer {
    private Pattern re_occurs = Pattern.compile("OCCURS\\s+(\\d+)\\s+TIMES");
    private List<CopyBookField> cbfields = new ArrayList<CopyBookField>();
    private int recordSize = 0;
    private CopyBookSerializationFormat format = CopyBookSerializationFormat.FULL;
    private Charset charset = StandardCharsets.UTF_8;
    private Map<CopyBookFieldType,CopyBookFieldFormat> paddingDefaults = new HashMap<>();

    public <T> CopyBookSerializer(Class<T> type) throws Exception {
        // Read all annotations recursively
        Stack<Class> classes = new Stack<>();
        classes.push(CopyBookDefaults.class); // Load defaults from interface
        classes.push(type);
        while (!classes.isEmpty()) {
            for (Annotation annotation : classes.pop().getAnnotations()) {
                if(CopyBook.class.isInstance(annotation)) {
                    this.format = ((CopyBook)annotation).format();
                    this.charset = Charset.forName(((CopyBook)annotation).charset());

                } else if (CopyBookFieldFormats.class.isInstance(annotation)){
                    for(CopyBookFieldFormat padding : ((CopyBookFieldFormats) annotation).value()) {
                        this.paddingDefaults.put(padding.fieldType(), padding);
                    }
                }
                if(!annotation.annotationType().getName().startsWith("java")) {
                    classes.push(annotation.annotationType());
                }
            }
        }

        // Walk class hierarchy
        this.cbfields = walkClass(type, new Field[0], new int[0], new int[0], new CopyBookField[0]);
        for(CopyBookField cbfield : cbfields) {
            cbfield.offset = recordSize;
            recordSize += cbfield.size;

            // Print copybook layout
            System.out.print("[" + Arrays.stream(cbfield.fields).map(Field::getName).collect(joining(", ")) + "]");
            System.out.print(", ");
            System.out.print(cbfield.size);
            System.out.print(", ");
            System.out.print("[" + Arrays.stream(cbfield.indexs).mapToObj(String::valueOf).collect(joining(", ")) + "]");
            System.out.print(", ");
            System.out.print("[" + Arrays.stream(cbfield.occurs).mapToObj(String::valueOf).collect(joining(", ")) + "]");
            System.out.println();
        }
    }

    // Walk and find all copybook annotations and flatten to a list of CopyBookfields
    private <T> List<CopyBookField> walkClass(Class<T> type, Field[] fields, int[] indexes, int[] occurs, CopyBookField[] counters) throws Exception {
        List<CopyBookField> results = new ArrayList<>();
        Map<String, CopyBookField> fieldnames = new HashMap<>();

        //TODO: Validate that copybook matches the fields

        // Itegrate over the class fields with CopyBookLine annotation
        for (Field field : type.getDeclaredFields()) {
            Class fieldclass = field.getType();
            CopyBookLine[] cbls = (CopyBookLine[])field.getAnnotationsByType(CopyBookLine.class);

            // Read annotations for padding of this field
            Map<CopyBookFieldType,CopyBookFieldFormat> fieldPaddings = new HashMap<>(paddingDefaults);
            for(CopyBookFieldFormat padding : (CopyBookFieldFormat[])field.getAnnotationsByType(CopyBookFieldFormat.class)) {
                fieldPaddings.put(padding.fieldType(), padding);
            }

            // Handle private fields
            if(!field.isAccessible()) {
                field.setAccessible(true);
            }

            // Append new field and index to arrays
            Field[] currentfields = arrayAppend(fields, field);

            // Append counter filed
            CopyBookField countercbf = fieldnames.get(field.getName() + "_count");
            CopyBookField[] currentcounters = arrayAppend(counters, countercbf);;

            if(cbls.length == 0) {
                // No CopyBookLine on this field

            } else if(cbls.length == 1) {
                System.out.println(new String(new char[currentfields.length * 2]).replace("\0", " ") + cbls[0].value());
                int occurscount = getOccurs(cbls[0].value());

                if(occurscount > 1) {
                    if(fieldclass.isArray() && fieldclass.getComponentType().getPackage() == type.getPackage()) { //TODO: Check if it has the copybook annotation instead of using package
                        // Array type in package
                        for (int i=0; i < occurscount; i++) {
                            results.addAll(walkClass(fieldclass.getComponentType(), currentfields, arrayAppend(indexes, i), arrayAppend(occurs, occurscount), currentcounters));
                        }
                    }

                } else if(fieldclass.getPackage() == type.getPackage()) { //TODO: Check if it has the copybook annotation instead of using package
                    // Complex type in package
                    results.addAll(walkClass(fieldclass, currentfields, arrayAppend(indexes, -1), arrayAppend(occurs, occurscount), currentcounters));

                } else {
                    // Simple types, such as int and String
                    CopyBookField cbf = new CopyBookField(cbls[0].value(), fieldPaddings);
                    cbf.fields = currentfields;
                    cbf.counters = currentcounters;
                    cbf.indexs = arrayAppend(indexes, -1);
                    cbf.occurs = arrayAppend(occurs, occurscount);
                    results.add(cbf);
                    fieldnames.put(field.getName(), cbf);

                    // Find field this field is a counter for and reference it
                    String name = field.getName();
                    if(name.endsWith("_count")) {
                        CopyBookField refcbf = fieldnames.get(name.substring(name.length() - 6));
                        if(refcbf != null) {
                            refcbf.counters[refcbf.counters.length -1] = cbf;
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
                        CopyBookField cbf = new CopyBookField(cbls[1].value(), fieldPaddings);
                        cbf.fields = currentfields;
                        cbf.counters = currentcounters;
                        cbf.indexs = arrayAppend(indexes, i);
                        cbf.occurs = arrayAppend(occurs, occurscount);
                        results.add(cbf);
                        fieldnames.put(field.getName(), cbf);

                        // Find field this field is a counter for and reference it
                        String name = field.getName();
                        if(name.endsWith("_count")) {
                            CopyBookField refcbf = fieldnames.get(name.substring(name.length() - 6));
                            if(refcbf != null) {
                                refcbf.counters[refcbf.counters.length -1] = cbf;
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

    public <T> byte[] serialize(T obj) throws CopyBookException, InstantiationException, IllegalAccessException {
        if(this.format == CopyBookSerializationFormat.FULL) {
            return serializeFull(obj);

        } else if (this.format == CopyBookSerializationFormat.PACKED) {
            return serializePacked(obj);

        } else {
            throw new CopyBookException("Unsupported format");
        }
    }

    private <T> byte[] serializeFull(T obj) throws CopyBookException, IllegalAccessException {
        ByteBuffer buf = ByteBuffer.wrap(new byte[this.recordSize]);
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
                    Arrays.fill(result, cbfield.padding);
                    if(cbfield.rightpadding) {
                        System.arraycopy(bytes, 0, result, 0, bytes.length);
                    } else {
                        System.arraycopy(bytes, 0, result, result.length - bytes.length, bytes.length);
                    }

                } else {
                    throw new CopyBookException("Field '"+ cbfield.getFieldName() +"' to long : " + bytes.length + " > " + cbfield.size);
                }

                buf.put(result);
                System.out.println(cbfield.type + "(" + cbfield.size + ","+ result.length +"): " + current.toString());

            } else {
                // Write empty space for missing obj
                byte[] filler = new byte[cbfield.size];
                Arrays.fill(filler, cbfield.padding);
                buf.put(filler);
                System.out.println(cbfield.type.name() + "(" + cbfield.size + "): " + "______");
            }
        }

        return buf.array();
    }

    private <T> byte[] serializePacked(T obj) {
        return null;
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
        if(this.format == CopyBookSerializationFormat.FULL) {
            return deserializeFull(data, type);

        } else if (this.format == CopyBookSerializationFormat.PACKED) {
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
                String strvalue = new String(ByteUtils.trim(bytevalue, cbfield.padding, cbfield.rightpadding), charset);

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
                            } else if (fieldtype.equals(BigInteger.class)) {
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
                            } else if (fieldtype.equals(BigDecimal.class)) {
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

                    int[] sizes = new int[cbfield.counters.length];
                    for (int i = 0; i < cbfield.counters.length; i++) {
                        CopyBookField counter = cbfield.counters[i];
                        if (counter != null) {
                            strvalue = new String(ByteUtils.trim(Arrays.copyOfRange(data, counter.offset, counter.offset + counter.size), counter.padding, counter.rightpadding), charset);
                            sizes[i] = Integer.parseInt(strvalue);
                        }
                    }

                    cbfield.set(obj, result, true, sizes); // TODO: Validate that sizes are not bigger than allowed

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