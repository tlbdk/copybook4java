package dk.nversion.copybook;

import dk.nversion.ByteUtils;

import java.io.IOException;
import java.io.InputStream;
import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
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
    private int maxRecordSize = 0;
    private int packingItemsCount;
    private int bitmapMaxBlocks;
    private int bitmapMaxSize;
    private int separatorsSize;

    public <T> CopyBookSerializer(Class<T> type) throws CopyBookException {
        this(type, false);
    }

    public <T> CopyBookSerializer(Class<T> type, boolean debug) throws CopyBookException {
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
            cbfield.offset = maxRecordSize;
            maxRecordSize += cbfield.size;

            // Count the number fields that can be packed with 1 level packing
            if(!cbfield.fields[0].equals(lastRootField)) {
                if(cbfield.isArray(0)) {
                    packingItemsCount += cbfield.occurs[0];
                } else {
                    packingItemsCount++;
                }
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
                System.out.print(", ");
                System.out.print("[" + Arrays.stream(cbfield.subFields).mapToObj(String::valueOf).collect(joining(", ")) + "]");
                System.out.println();
            }
        }

        // Calculate sizes for packing
        bitmapMaxBlocks = packingItemsCount / (bitmapBlockSize * 8 - 1) + 1;
        bitmapMaxSize = bitmapBlockSize * bitmapMaxBlocks;
        separatorsSize = packingItemsCount;
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
    private <T> List<CopyBookField> walkClass(Class<T> type, Field[] fields, int[] indexes, int[] occurs, CopyBookField[] counters) throws CopyBookException {
        List<CopyBookField> results = new ArrayList<>();
        List<CopyBookField> subResults;
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
                            subResults = walkClass(fieldClass.getComponentType(), currentfields, arrayAppend(indexes, i), arrayAppend(occurs, occurscount), currentcounters);
                            for(int j=0; j < subResults.size(); j++) {
                                subResults.get(j).subFields = arrayPrepend(subResults.get(j).subFields, subResults.size() - j - 1);
                            }
                            results.addAll(subResults);
                        }
                    } else {
                        throw new CopyBookException("Field '" + getFullFieldName(currentfields) + "' should be an array type with an CopyBook annotation");
                    }

                } else if(fieldClass.getAnnotation(CopyBook.class) != null) {
                    // Complex type in package
                    subResults = walkClass(fieldClass, currentfields, arrayAppend(indexes, -1), arrayAppend(occurs, occurscount), currentcounters);
                    for(int j=0; j < subResults.size(); j++) {
                        subResults.get(j).subFields = arrayPrepend(subResults.get(j).subFields, subResults.size() - j - 1);
                    }
                    results.addAll(subResults);

                } else {
                    // Simple types, such as int and String
                    CopyBookField cbf = new CopyBookField(cbls[0].value(), charset, currentfields, currentcounters, arrayAppend(indexes, -1), arrayAppend(occurs, occurscount), fieldPaddings);
                    cbf.subFields = new int[] { 0 };
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
                        cbf.subFields = new int[] { 0 };
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
                    throw new CopyBookException("Field is missing CopyBookLine with OCCURS");
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
        ByteBuffer buf = ByteBuffer.wrap(new byte[this.maxRecordSize]);
        for(CopyBookField cbfield : cbfields) {
            buf.put(cbfield.getBytes(obj, true));
        }
        return buf.array();
    }

    private <T> byte[] serializePacked(T obj) throws CopyBookException, IllegalAccessException {
        // Init byte array
        byte[] dataBytes = new byte[separatorsSize + this.maxRecordSize]; // Calculate max size of data bytes
        byte[] bitMapBytes = new byte[bitmapMaxSize];

        // Write values to bytes after the bitmap blocks
        ByteBuffer buf = ByteBuffer.wrap(dataBytes);
        int bitIndex = 0;
        int maxBit = 0;
        for(int i=0; i < cbfields.size(); i++) {
            CopyBookField cbfield = cbfields.get(i);
            byte strBytes[];

            String cbline = cbfield.line;

            if (cbfield.fields.length == 1) { // Simple and Array of Simple
                strBytes = cbfield.getBytes(obj, false);
                if(strBytes != null) {
                    if(ByteUtils.indexOf(strBytes, separatorByte, 0, strBytes.length) < 0) {
                        setBitInBitmap(bitMapBytes, bitIndex);
                        maxBit = bitIndex;
                        buf.put(strBytes);
                        buf.put(separatorByte);

                    } else {
                        throw new CopyBookException("Field '"+ cbfield.getFieldName() + "' contains the separator char");
                    }
                }
                bitIndex++;

            } else { // Nested Object or Array of Object
                boolean isLastFieldInRootObj = i + 1 == cbfields.size() // Last item in for loop
                        || !cbfields.get(i + 1).fields[0].equals(cbfield.fields[0]) // New root object on next iteration
                        || cbfields.get(i + 1).indexes[0] != cbfield.indexes[0];  // New index in root object on next iteration

                if(!cbfield.isNull(obj, 0)) { // If root object is not null then write bytes
                    if (isLastFieldInRootObj)  // New index in root object on next iteration
                    {
                        strBytes = cbfield.getBytes(obj, false); // Don't pad if it's the last field
                        if(strBytes == null) {
                            strBytes = cbfield.getBytes(obj, true); // Except if it's null
                        }
                        buf.put(strBytes);
                        setBitInBitmap(bitMapBytes, bitIndex);
                        maxBit = bitIndex;
                        buf.put(separatorByte);


                    } else {
                        strBytes = cbfield.getBytes(obj, true);
                        buf.put(strBytes);
                    }

                    if (ByteUtils.indexOf(strBytes, separatorByte, 0, strBytes.length) > -1) {
                        throw new CopyBookException("Field '" + cbfield.getFieldName() + "' contains the separator char");
                    }

                } else {
                    // Skip fields since the root item is null
                    i += cbfield.subFields[0];
                    bitIndex++;
                }

                if(isLastFieldInRootObj) {
                    bitIndex++;
                }
            }
        }


        // Trim unused bytes before returning the array
        int bitMapBlocks = maxBit / (bitmapBlockSize * 8 - 1) + 1;
        int bitMapSize = bitMapBlocks * bitmapBlockSize;

        // Set bit 64(bit index 63) to 1
        for(int i = 0; i < bitMapSize / bitmapBlockSize  - 1; ++i) {
            bitMapBytes[i * bitmapBlockSize + (bitmapBlockSize - 1)] = (byte)(bitMapBytes[i * bitmapBlockSize + (bitmapBlockSize - 1)] | 1); // 1 decimals = 00000001 binary
        }

        byte[] result = new byte[buf.position() + bitMapSize];
        System.arraycopy(bitMapBytes, 0, result, 0, bitMapSize); // Copy bitmap to result array
        System.arraycopy(dataBytes, 0, result, bitMapSize, buf.position());

        return result;
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
    private void setBitInBitmap(byte[] bytes, int bitindex) {
        bitindex += bitindex / (this.bitmapBlockSize * 8 - 1);
        bytes[bitindex / this.bitmapBlockSize] = (byte)(bytes[bitindex / this.bitmapBlockSize] | (128 >> (bitindex % 8))); // 128 decimals = 10000000 binary
    }

    private boolean getBitInBitmap(byte[] bytes, int bitIndex, int bitmapSize) {
        bitIndex += bitIndex / 63;
        if(bitIndex < bitmapSize * 8) {
            return (bytes[bitIndex / this.bitmapBlockSize] & (128 >> (bitIndex % this.bitmapBlockSize))) != 0; // 128 decimals = 10000000 binary
        } else {
            return false;
        }
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

    private int[] arrayPrepend(int[] src, int obj) {
        int[] dst = new int[src.length + 1];
        System.arraycopy(src, 0, dst, 1, src.length);
        dst[0] = obj;
        return dst;
    }

    private int getOccurs(String str) {
        Matcher matcher = re_occurs.matcher(str);
        if(matcher.find()) {
            return Integer.parseInt(matcher.group(1));
        } else {
            return -1;
        }
    }

    public int getMaxRecordSize() {
        return this.maxRecordSize;
    }

    public <T> T deserialize(InputStream inputStream, Class<T> type) throws CopyBookException, InstantiationException, IllegalAccessException, IOException {
        return deserialize(ByteUtils.toByteArray(inputStream), type);
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
        if(data.length != maxRecordSize) {
            throw new CopyBookException("Data length does not match the size of the copybook : " + data.length + " vs " + maxRecordSize);
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
        int bitMapSize = getBitMapSize(data);
        byte[] bitmapBytes = new byte[bitMapSize];
        buf.get(bitmapBytes);
        int bitIndex = 0;
        for (int i = 0; i < cbfields.size(); i++) {
            CopyBookField cbfield = cbfields.get(i);
            String debugBitmapString = debugBitmap(bitmapBytes, 0, 8);
            String cbline = cbfield.line;

            // Find size of packed array
            int[] sizeHints = new int[cbfield.fields.length];
            Arrays.fill(sizeHints, -1);
            if (cbfield.isArray(0) && cbfield.indexes[0] == 0) { // Is array and first element
                for (int j = 0; j < cbfield.occurs[0]; j++) {
                    if (getBitInBitmap(bitmapBytes, bitIndex + j, bitMapSize)) {
                        sizeHints[0] = j + 1;
                    }
                }
            }

            // Read fields
            if (cbfield.fields.length == 1) { // Simple and Simple Array
                if (getBitInBitmap(bitmapBytes, bitIndex, bitMapSize)) {
                    int index = ByteUtils.indexOf(data, separatorByte, buf.position() + 1, cbfield.size);
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
                boolean isLastFieldInRootObj = i + 1 == cbfields.size() // Last item in for loop
                        || !cbfields.get(i + 1).fields[0].equals(cbfield.fields[0]) // New root object on next iteration
                        || cbfields.get(i + 1).indexes[0] != cbfield.indexes[0];  // New index in root object on next iteration

                if (getBitInBitmap(bitmapBytes, bitIndex, bitMapSize)) {
                    // Read field size from buf if it's set in the bitmap
                    byte[] byteValue;
                    if(isLastFieldInRootObj) {
                        int index = ByteUtils.indexOf(data, separatorByte, buf.position() + 1, cbfield.size);
                        if(index > 0) {
                            byteValue = new byte[index - buf.position()];

                        } else {
                            throw new CopyBookException("Could not find expected separator at index " + buf.position());
                        }
                    } else {
                        byteValue = new byte[cbfield.size];
                    }

                    buf.get(byteValue);
                    cbfield.set(obj, byteValue, true, true, sizeHints); // Fixed length fields

                } else {
                    // Skip fields since the root item is null
                    i += cbfield.subFields[0];
                    bitIndex++;
                }

                // Check if last field in this root object or this is end of list
                if (isLastFieldInRootObj) {
                    if (getBitInBitmap(bitmapBytes, bitIndex, bitMapSize)) {
                        buf.position(buf.position() + 1);
                    }
                    bitIndex++;
                }
            }
        }

        return obj;
    }

    private int getBitMapSize(byte[] data) throws CopyBookException {
        // Read bit map size by comparing the last bit in each block
        int e;
        for(e = 1; (data[e * 8 - 1] & 1) != 0; ++e) { }
        int bitMapSize = e * bitmapBlockSize;
        if(bitMapSize > bitmapMaxSize) {
            throw new CopyBookException("Bitmap is to large for this copybook");
        }

        return bitMapSize;
    }
}