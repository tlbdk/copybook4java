package dk.nversion.copybook;

import dk.nversion.ByteUtils;

import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.math.BigDecimal;
import java.nio.ByteBuffer;
import java.nio.charset.Charset;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static java.util.stream.Collectors.joining;

public class CopyBookSerializer {
    private Class type;
    private Pattern re_occurs = Pattern.compile("OCCURS\\s+(\\d+)\\s+TIMES");
    private List<CopyBookField> cbfields = new ArrayList<CopyBookField>();
    private boolean debug = false;

    // Configuration
    private CopyBookSerializationFormat format;
    private Charset charset;
    private Map<CopyBookFieldType,CopyBookFieldFormat> paddingDefaults = new HashMap<>();
    // For Packing
    private byte separatorByte = '\u000b'; // Vertical tab
    private int bitmapBlockSize = 8; // Allocate 8 bytes for bit map of what fields are in use, bit 64 show if another 8 bytes has been allocated

    // Calculated fields
    private int recordSize = 0;
    private int packingItemsCount;
    private int bitmapBlocks;
    private int bitmapSize;
    private int separatorsSize;

    public <T> CopyBookSerializer(Class<T> type) throws Exception {
        this(type, false);
    }

    public <T> CopyBookSerializer(Class<T> type, boolean debug) throws Exception {
        this.debug = debug;
        this.type = type;

        // Read copybook annotations and defaults
        List<CopyBook> copybookAnnotations = getAnnotationsRecursively(CopyBookDefaults.class, CopyBook.class);
        copybookAnnotations.addAll(getAnnotationsRecursively(type, CopyBook.class));
        for(CopyBook annotation : copybookAnnotations) {
            if (annotation.format() != CopyBookSerializationFormat.NONE) {
                format = annotation.format();
            }
            if (!annotation.charset().isEmpty()) {
                charset = Charset.forName(annotation.charset());
            }
            if(annotation.separatorChar() != 'G') { // Java sucks and does not support null as default value for annotations so we pick large G as this is å in EBCDIC and unlikely to be used as separator char
                separatorByte = (byte)annotation.separatorChar();
            }
            if(annotation.bitmapBlockSize() != 0) {
                bitmapBlockSize = annotation.bitmapBlockSize();
            }
        }

        // Read copybook field annotations
        List<CopyBookFieldFormats> copybookFieldAnnotations = getAnnotationsRecursively(CopyBookDefaults.class, CopyBookFieldFormats.class);
        copybookFieldAnnotations.addAll(getAnnotationsRecursively(type, CopyBookFieldFormats.class));
        for(CopyBookFieldFormats annotations : copybookFieldAnnotations) {
            for(CopyBookFieldFormat annotation : annotations.value()) {
                paddingDefaults.put(annotation.fieldType(), annotation);
            }
        }
        // Handle case where there is only one annotation
        for(CopyBookFieldFormat annotation : getAnnotationsRecursively(type, CopyBookFieldFormat.class)) {
            paddingDefaults.put(annotation.fieldType(), annotation);
        }


        // Walk class hierarchy
        this.cbfields = walkClass(type, new Field[0], new int[0], new int[0], new CopyBookField[0]);

        // Iterate over list and count size of array
        Field lastRootField = null;
        packingItemsCount = 0;
        for(CopyBookField cbfield : cbfields) {
            cbfield.offset = recordSize;
            recordSize += cbfield.size;

            // Count the number fields that can be packed with 1 level packing
            if(!cbfield.fields[0].equals(lastRootField) || cbfield.isArray(0)) {
                packingItemsCount++;
                cbfield.packingItem = true;
            }
            lastRootField = cbfield.fields[0];

            if(debug) {
                // Print copybook layout
                System.out.print("[" + Arrays.stream(cbfield.fields).map(Field::getName).collect(joining(", ")) + "]");
                System.out.print(", ");
                System.out.print(cbfield.size);
                System.out.print(", ");
                System.out.print("[" + Arrays.stream(cbfield.indexes).mapToObj(String::valueOf).collect(joining(", ")) + "]");
                System.out.print(", ");
                System.out.print("[" + Arrays.stream(cbfield.occurs).mapToObj(String::valueOf).collect(joining(", ")) + "]");
                System.out.println();
            }
        }

        // Calculate sizes for packing
        bitmapBlocks = this.packingItemsCount / (bitmapBlockSize * 8 - 1) + 1;
        bitmapSize = bitmapBlockSize * bitmapBlocks;
        separatorsSize = this.packingItemsCount;
    }

    private <T extends Annotation> List<T> getAnnotationsRecursively(Class type, Class<T> annotationType) {
        List<Annotation> results = new ArrayList<>();
        for (Annotation annotation : type.getAnnotations()) {
            if(annotationType.isInstance(annotation)) {
                results.add(annotation);

            } else if (!annotation.annotationType().getName().startsWith("java")) {
                results.addAll(getAnnotationsRecursively(annotation.annotationType(), annotationType));
            }
        }
        return (List<T>)results;
    }

    // Walk and find all copybook annotations and flatten to a list of CopyBookfields
    private <T> List<CopyBookField> walkClass(Class<T> type, Field[] fields, int[] indexes, int[] occurs, CopyBookField[] counters) throws Exception {
        List<CopyBookField> results = new ArrayList<>();
        Map<String, CopyBookField> fieldNames = new HashMap<>();

        // Iterate over the class fields with CopyBookLine annotation
        for (Field field : type.getDeclaredFields()) {
            Class fieldClass = field.getType();
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
            CopyBookField countercbf = fieldNames.get(field.getName() + "_count");
            CopyBookField[] currentcounters = arrayAppend(counters, countercbf);;

            if(cbls.length == 0) {
                // No CopyBookLine on this field

            } else if(cbls.length == 1) {
                if(debug) {
                    System.out.println(new String(new char[currentfields.length * 2]).replace("\0", " ") + cbls[0].value());
                }
                int occurscount = getOccurs(cbls[0].value());
                if(occurscount > 1) {
                    if(fieldClass.isArray() && fieldClass.getComponentType().getAnnotation(CopyBook.class) != null) {
                        // Array type in package
                        for (int i=0; i < occurscount; i++) {
                            results.addAll(walkClass(fieldClass.getComponentType(), currentfields, arrayAppend(indexes, i), arrayAppend(occurs, occurscount), currentcounters));
                        }
                    } else {
                        throw new CopyBookException("Field '" + getFullFieldName(currentfields) + "' should be an array type with an CopyBook annotation");
                    }

                } else if(fieldClass.getAnnotation(CopyBook.class) != null) {
                    // Complex type in package
                    results.addAll(walkClass(fieldClass, currentfields, arrayAppend(indexes, -1), arrayAppend(occurs, occurscount), currentcounters));

                } else {
                    // Simple types, such as int and String
                    CopyBookField cbf = new CopyBookField(cbls[0].value(), charset, currentfields, currentcounters, arrayAppend(indexes, -1), arrayAppend(occurs, occurscount), fieldPaddings);
                    results.add(cbf);
                    fieldNames.put(field.getName(), cbf);

                    // Find field this field is a counter for and reference it
                    String name = field.getName();
                    if(name.endsWith("_count")) {
                        CopyBookField refcbf = fieldNames.get(name.substring(name.length() - 6));
                        if(refcbf != null) {
                            refcbf.setCounter(cbf);
                        }
                    }
                }

            } else if(cbls.length == 2) {
                if(debug) {
                    System.out.println(new String(new char[currentfields.length * 2]).replace("\0", " ") + cbls[0].value());
                }
                int occursCount = getOccurs(cbls[0].value());
                if(occursCount > 1) {
                    // Simple array types, such as int[] and String[]
                    for (int i = 0; i < occursCount; i++) {
                        if(debug) {
                            System.out.println(new String(new char[currentfields.length * 2 + 2]).replace("\0", " ") + cbls[1].value());
                        }
                        CopyBookField cbf = new CopyBookField(cbls[1].value(), charset, currentfields, currentcounters, arrayAppend(indexes, i), arrayAppend(occurs, occursCount), fieldPaddings);

                        results.add(cbf);
                        fieldNames.put(field.getName(), cbf);

                        // Find field this field is a counter for and reference it
                        String name = field.getName();
                        if(name.endsWith("_count")) {
                            CopyBookField refcbf = fieldNames.get(name.substring(name.length() - 6));
                            if(refcbf != null) {
                                refcbf.setCounter(cbf); ;
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
            buf.put(cbfield.getBytes(obj, true));
        }
        return buf.array();
    }

    private <T> byte[] serializePacked(T obj) throws CopyBookException, IllegalAccessException {
        // Init byte array
        byte[] bytes = new byte[bitmapSize + separatorsSize + this.recordSize]; // Calculate max size of bytes

        // Set bit 64(bit index 63) to 1
        for(int i = 0; i < bitmapBlocks - 1; ++i) {
            bytes[i * bitmapBlockSize + (bitmapBlockSize - 1)] = 1; // 1 decimals = 00000001 binary
        }

        // Write values to bytes after the bitmap blocks
        ByteBuffer buf = ByteBuffer.wrap(bytes, bitmapBlocks * bitmapBlockSize, bytes.length - bitmapBlocks * bitmapBlockSize);
        int bitIndex = 0;
        int lastRootIndex = cbfields.get(0).getIndex(0);
        Field lastRootField = cbfields.get(0).fields[0];
        for(int i=0; i < cbfields.size(); i++) {
            CopyBookField cbfield = cbfields.get(i);
            byte strBytes[];


            if (cbfield.fields.length == 1) { // Simple and Array of Simple
                strBytes = cbfield.getBytes(obj, false);
                if(strBytes != null) {
                    if(ByteUtils.indexOf(strBytes, separatorByte, 0, strBytes.length) < 0) {
                        setBitInBitmap(bytes, bitIndex, bitmapBlockSize);
                        buf.put(strBytes);
                        buf.put(separatorByte);

                    } else {
                        throw new CopyBookException("Field '"+ cbfield.getFieldName() + "' contains the separator char");
                    }
                }
                bitIndex++;

            } else { // Object and Array of Object
                if(cbfield.fields[0].get(obj) != null) {
                    strBytes = cbfield.getBytes(obj, true);
                    if(ByteUtils.indexOf(strBytes, separatorByte, 0, strBytes.length) < 0) {
                        buf.put(strBytes);

                        // Check if last field in this root object or this is end of list
                        if (i + 1 == cbfields.size() || !cbfields.get(i + 1).fields[0].equals(cbfield.fields[0]) || cbfields.get(i + 1).indexes[0] != cbfield.indexes[0]) {
                            setBitInBitmap(bytes, bitIndex, 8);
                            buf.put(separatorByte);
                            bitIndex++;
                        }

                    } else {
                        throw new CopyBookException("Field '"+ cbfield.getFieldName() + "' contains the separator char");
                    }
                }
            }
        }

        // Trim bytes when returning the array
        return Arrays.copyOf(bytes, buf.position());
    }

    private static String getFullFieldName(Field[] fields) {
        String result = fields[0].getName();
        for(int i=1; i <= fields.length - 1 ; i++) {
            result += '.' + fields[i].getName();
        }
        return result;
    }

    private String debugBitmap(byte[] bytes, int index, int length) {
        String result = "";
        for(int i = index; i < length; i++) {
            result += ("0000000" + Integer.toBinaryString(bytes[i] & 0xFF)).replaceAll(".*(.{8})$", "$1");
        }
        return result;
    }

    // Sets 63 bits(bit index 0-62) in 8 bytes N times where bit 64(bit index 63) tells if a new 8 bytes block is used
    private void setBitInBitmap(byte[] bytes, int bitindex, int blocksize) {
        bitindex += bitindex / (blocksize * 8 - 1);
        bytes[bitindex / blocksize] = (byte)(bytes[bitindex / blocksize] | (128 >> (bitindex % 8))); // 128 decimals = 10000000 binary
    }

    private boolean getBitInBitmap(byte[] bytes, int bitIndex, int blockSize) {
        bitIndex += bitIndex / 63;
        return (bytes[bitIndex / blockSize] & (128 >> (bitIndex % blockSize))) != 0; // 128 decimals = 10000000 binary
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

    public int getRecordSize () {
        return this.recordSize;
    }

    public <T> T deserialize(byte[] data, Class<T> type) throws CopyBookException, InstantiationException, IllegalAccessException {
        if(!this.type.equals(type)) {
            throw new CopyBookException("Type needs to match the type used when constructing the serializer");
        }

        if(this.format == CopyBookSerializationFormat.FULL) {
            return deserializeFull(data, type);

        } else if (this.format == CopyBookSerializationFormat.PACKED) {
            return deserializePacked(data, type);

        } else {
            throw new CopyBookException("Unsupported format");
        }
    }

    private <T> T deserializeFull(byte[] data, Class<T> type) throws CopyBookException, InstantiationException, IllegalAccessException {
        if(data.length != recordSize) {
            throw new CopyBookException("Data length does not match the size of the copybook : " + data.length + " vs " + recordSize);
        }
        T obj = type.newInstance();
        ByteBuffer buf = ByteBuffer.wrap(data);

        CBFIELDS:
        for (CopyBookField cbfield : cbfields) {
            // Convert field bytes to string and trim value
            byte[] byteValue = new byte[cbfield.size];
            buf.get(byteValue);

            int[] sizeHints = new int[cbfield.counters.length];
            Arrays.fill(sizeHints, -1);
            for (int i = 0; i < cbfield.counters.length; i++) {
                CopyBookField counter = cbfield.counters[i];
                if (counter != null) {
                    sizeHints[i] = (int) counter.get(obj);

                    // Validate that this cbfield's index is within the size hint of the array
                    if (cbfield.indexes[i] > 0 && cbfield.indexes[i] >= sizeHints[i]) {
                        continue CBFIELDS; // Skip this cbfield
                    }
                }
            }

            cbfield.set(obj, byteValue, true, true, sizeHints);
        }

        return obj;
    }

    private <T> T deserializePacked(byte[] data, Class<T> type) throws CopyBookException, InstantiationException, IllegalAccessException {
        T obj = type.newInstance();
        ByteBuffer buf = ByteBuffer.wrap(data);
        byte[] bitmapBytes = new byte[bitmapSize];
        buf.get(bitmapBytes);
        int bitIndex = 0;
        for (int i = 0; i < cbfields.size(); i++) {
            CopyBookField cbfield = cbfields.get(i);
            String test = debugBitmap(bitmapBytes, 0, 8);

            // Find size of packed array
            int[] sizeHints = new int[cbfield.fields.length];
            if (cbfield.isArray(0) && cbfield.indexes[0] == 0) { // Is array and first element
                for (int j = 0; j < cbfield.occurs[0]; j++) {
                    if (getBitInBitmap(bitmapBytes, bitIndex + j, bitmapBlockSize)) {
                        sizeHints[0] = j + 1;
                    }
                }
            }

            // Read fields
            if (cbfield.fields.length == 1) { // Simple and Simple Array
                if (getBitInBitmap(bitmapBytes, bitIndex, bitmapBlockSize)) {
                    int index = ByteUtils.indexOf(data, separatorByte, buf.position(), Math.min(data.length, cbfield.size));
                    if(index > 0) {
                        byte[] byteValue = new byte[index - buf.position()];
                        buf.get(byteValue);
                        buf.position(buf.position() + 1); // Skip separatorByte
                        cbfield.set(obj, byteValue, true, false, sizeHints); // Variable length fields with separatorByte

                    } else {
                        throw new CopyBookException("Could not find expected separator in response at index " + buf.position());
                    }
                }
                bitIndex++;

            } else { // Object and Object Array
                if (getBitInBitmap(bitmapBytes, bitIndex, bitmapBlockSize)) {
                    // Read field size from buf if it's set in the bitmap
                    byte[] bytevalue = new byte[cbfield.size];
                    buf.get(bytevalue);
                    cbfield.set(obj, bytevalue, true, true, sizeHints); // Fixed length fields
                }

                // Check if last field in this root object or this is end of list
                if (i + 1 == cbfields.size() || !cbfields.get(i + 1).fields[0].equals(cbfield.fields[0]) || cbfields.get(i + 1).indexes[0] != cbfield.indexes[0]) {
                    bitIndex++;
                    buf.position(buf.position() + 1); // Skip separatorByte
                }
            }
        }

        return obj;
    }
}