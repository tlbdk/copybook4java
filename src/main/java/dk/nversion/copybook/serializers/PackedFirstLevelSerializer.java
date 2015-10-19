package dk.nversion.copybook.serializers;

import dk.nversion.ByteUtils;
import dk.nversion.copybook.exceptions.CopyBookException;

import java.nio.ByteBuffer;
import java.util.HashMap;
import java.util.List;

public class PackedFirstLevelSerializer extends CopyBookSerializerBase {
    private int maxBitmapSize;
    private byte separatorByte;
    private int bitmapBlockSize;

    public PackedFirstLevelSerializer(CopyBookSerializerConfig config) {
        super(config);
        this.bitmapBlockSize =  8;//config.getBitmapBlockSize();
        int packingItemsCount = countPackingItems(config.getFields());
        int maxBitmapBlocks = packingItemsCount / (bitmapBlockSize * 8 - 1) + 1;
        this.maxBitmapSize = bitmapBlockSize * maxBitmapBlocks;
        this.separatorByte = '\u000b';//config.getSeparatorByte();
    }

    private int countPackingItems(List<CopyBookField> fields) {
        int count = 0;
        for(CopyBookField field : fields) {
            if(field.isArray()) {
                count += field.getMaxOccurs();

            } else {
                count++;
            }
        }
        return count;
    }

    @Override
    public <T> byte[] serialize(T obj) throws CopyBookException {
        PackedBuffer buffer = new PackedBuffer(this.maxRecordSize, this.maxBitmapSize, this.bitmapBlockSize, this.separatorByte);
        writeFields(buffer, this.fields, obj, true);
        return buffer.array();
    }

    private void writeFields(PackedBuffer buffer, List<CopyBookField> fields, Object rootObj, boolean rootLast) throws CopyBookException {
        for(CopyBookField field : fields) {
            boolean last = rootLast && field.isLast();
            if(field.isArray()) {
                Object array = field.getObject(rootObj);
                if(field.hasSubCopyBookFields()) {
                    // Complex array types fx. Request[]
                    if(field.getLevel() == 0) {
                        for(int j = 0; j < field.getMaxOccurs(); j++) {
                            Object item = array != null ? field.getObject(rootObj, array, j) : null;
                            if(item != null) {
                                writeFields(buffer, field.getSubCopyBookFields(), item, true);

                            } else {
                                buffer.put(null, true); // Increment bitmapIndex
                            }
                        }

                    } else {
                        for(int j = 0; j < field.getMaxOccurs(); j++) {
                            Object item = array != null ? field.getObject(rootObj, array, j) : null;
                            writeFields(buffer, field.getSubCopyBookFields(), item, last && field.getMaxOccurs() - 1 == j);
                        }
                    }

                } else {
                    // Simple array types, fx. int[]
                    for(int j = 0; j < field.getMaxOccurs(); j++) {
                        writeField(buffer, field, field.getObject(rootObj, array, j), last && field.getMaxOccurs() - 1 == j);
                    }
                }

            } else if(field.hasSubCopyBookFields()) {
                // Complex type fx, Request
                Object item = field.getObject(rootObj);
                if(field.getLevel() == 0) {
                    if(item != null) {
                        writeFields(buffer, field.getSubCopyBookFields(), item, true);

                    } else {
                        buffer.put(null, true); // write separator
                    }

                } else {
                    writeFields(buffer, fields, item, last);
                }

            } else {
                // Simple type fx. int, String or types we support with TypeConverters
                writeField(buffer, field, field.getObject(rootObj), last);
            }
        }
    }

    private void writeField(PackedBuffer buffer, CopyBookField field, Object valueObj, boolean last) throws CopyBookException {
        if(field.getLevel() == 0) {
            buffer.put(field.getBytes(null, valueObj, false), true); // No padding with separator

        } else if(last) {
            byte[] valueBytes = field.getBytes(null, valueObj, false); // Don't pad if it's the last field
            if(valueBytes == null) {
                valueBytes = field.getBytes(null, valueObj, true); // Except if it's null
            }
            buffer.put(valueBytes, true); // With separator

        } else {
            buffer.put(field.getBytes(null, valueObj, true), false); // With padding without separator
        }
    }

    @Override
    public <T> T deserialize(byte[] bytes, Class<T> type) throws CopyBookException, InstantiationException {
        try {
            T obj = type.newInstance();
            PackedBuffer buffer = new PackedBuffer(bytes, this.maxBitmapSize, this.bitmapBlockSize, this.separatorByte);
            readFields(this.fields, buffer, obj, true);
            return obj;

        } catch (IllegalAccessException | InstantiationException e) {
            throw new CopyBookException("Failed to create new object", e);
        }
    }

    private void readFields(List<CopyBookField> fields, PackedBuffer buffer, Object obj, boolean rootLast) throws CopyBookException {
        for(CopyBookField field : fields) {
            boolean last = rootLast && field.isLast();

            if(field.isArray()) {
                int arraySize = buffer.getArraySize(field.getMaxOccurs());
                Object array = field.createArrayObject(obj, arraySize);
                if(field.hasSubCopyBookFields()) {
                    // Complex array types fx. Request[]
                    if (field.getLevel() == 0) {
                        for(int j = 0; j < arraySize; j++) {
                            readFields(fields, buffer, obj, true);
                        }

                    } else {
                        for(int j = 0; j < arraySize; j++) {
                            readFields(fields, buffer, obj, last);
                        }
                    }
                } else {
                    // Simple array types, fx. int[]
                    if(field.getLevel() == 0 || last) {
                        for(int j = 0; j < arraySize; j++) {
                            byte[] valueBytes = buffer.get(field.getSize(), true);
                            field.setBytes(array, valueBytes, 0, valueBytes.length, false);
                        }

                    } else {
                        for(int j = 0; j < arraySize; j++) {
                            byte[] valueBytes = buffer.get(field.getSize(), false);
                            field.setBytes(array, valueBytes, 0, valueBytes.length, true);
                        }
                    }
                }

            } else {
                if(field.hasSubCopyBookFields()) {
                    // Complex type fx, Request
                    if (field.getLevel() == 0) {
                        readFields(fields, buffer, obj, true);

                    } else {
                        readFields(fields, buffer, obj, last);
                    }

                } else {
                    // Simple type fx. int, String or types we support with TypeConverters
                    if(field.getLevel() == 0 || last) {
                        byte[] valueBytes = buffer.get(field.getSize(), true);
                        field.setBytes(obj, valueBytes, 0, valueBytes.length, false);

                    } else {
                        byte[] valueBytes = buffer.get(field.getSize(), false);
                        field.setBytes(obj, valueBytes, 0, valueBytes.length, true);
                    }
                }
            }
        }
    }

    private class PackedBuffer {
        private ByteBuffer buffer;
        private byte[] bitmapBytes;
        private int bitmapIndex = 0;
        private int maxUsedBit = 0;
        private int bitmapBlockSize;
        private byte separatorByte;
        private int bitmapSize;
        private int bitmapMaxSize;

        public PackedBuffer(int size, int bitmapMaxSize, int bitmapBlockSize, byte separatorByte) throws CopyBookException {
            this.buffer = ByteBuffer.wrap(new byte[size]);
            this.bitmapBytes = new byte[bitmapMaxSize];
            this.bitmapBlockSize = bitmapBlockSize;
            this.separatorByte = separatorByte;
        }

        public PackedBuffer(byte[] bytes, int bitmapMaxSize, int bitmapBlockSize, byte separatorByte) throws CopyBookException {
            this.buffer = ByteBuffer.wrap(bytes);
            this.bitmapBlockSize = bitmapBlockSize;
            this.separatorByte = separatorByte;
            this.bitmapMaxSize = bitmapMaxSize;
            this.bitmapSize = getBitMapSize(bytes);
        }

        public int getArraySize(int maxOccurs) {
            int size = 0;
            for (int i = 0; i < maxOccurs; i++) {
                if (getBitInBitmap(bitmapBytes, bitmapIndex + i, bitmapSize)) {
                    size = i + 1;
                }
            }
            return size;
        }

        public byte[] get(int maxLength, boolean separator) throws CopyBookException {
            byte[] byteValue = null;
            if(separator) {
                if (getBitInBitmap(this.buffer.array(), this.bitmapIndex, this.bitmapSize)) {
                    int index = ByteUtils.indexOf(this.buffer.array(), this.separatorByte, this.buffer.position() + 1, maxLength);
                    if (index > 0) {
                        byteValue = new byte[index - this.buffer.position()];
                        this.buffer.get(byteValue);
                        this.buffer.position(this.buffer.position() + 1); // Skip separatorByte

                    } else {
                        throw new CopyBookException("Could not find expected separator in response at index " + buffer.position());
                    }
                }
                bitmapIndex++;

            } else {
                byteValue = new byte[maxLength];
                this.buffer.get(byteValue);
            }

            return byteValue;
        }

        public void put(byte [] bytes, boolean separator) throws CopyBookException {
            if(bytes != null) {
                if (ByteUtils.indexOf(bytes, this.separatorByte, 0, bytes.length) > -1) {
                    throw new CopyBookException("Bytes contains the separator char");
                }
                buffer.put(bytes);
                System.out.print("'" + new String(bytes) +"'");

                if(separator) {
                    System.out.println("[]");
                    setBitInBitmap();
                    this.maxUsedBit = bitmapIndex;
                    buffer.put(this.separatorByte);
                }
            }
            if(separator) {
                System.out.println("((");
                this.bitmapIndex++;
            }
        }

        public byte[] array() {
            // Trim unused bytes before returning the array
            int bitmapBlocks = this.maxUsedBit / (this.bitmapBlockSize * 8 - 1) + 1;
            int bitmapSize = bitmapBlocks * this.bitmapBlockSize;

            // Set bit 64(bit index 63) to 1
            for(int i = 0; i < bitmapSize / this.bitmapBlockSize  - 1; ++i) {
                this.bitmapBytes[i * this.bitmapBlockSize + (this.bitmapBlockSize - 1)] = (byte)(this.bitmapBytes[i * this.bitmapBlockSize + (this.bitmapBlockSize - 1)] | 1); // 1 decimals = 00000001 binary
            }

            byte[] result = new byte[buffer.position() + bitmapSize];
            System.arraycopy(bitmapBytes, 0, result, 0, bitmapSize); // Copy bitmap to result array
            System.arraycopy(buffer.array(), 0, result, bitmapSize, buffer.position());
            return result;
        }

        // Sets 63 bits(bit index 0-62) in 8 bytes N times where bit 64(bit index 63) tells if a new 8 bytes block is used
        private void setBitInBitmap() {
            int bitOffset = this.bitmapIndex / (this.bitmapBlockSize * 8 - 1) + this.bitmapIndex;
            this.bitmapBytes[bitOffset / this.bitmapBlockSize] = (byte)(this.bitmapBytes[bitOffset / this.bitmapBlockSize] | (128 >> (bitOffset % 8))); // 128 decimals = 10000000 binary
        }

        private boolean getBitInBitmap(byte[] bytes, int bitIndex, int bitmapSize) {
            bitIndex += bitIndex / 63;
            if(bitIndex < bitmapSize * 8) {
                return (bytes[bitIndex / this.bitmapBlockSize] & (128 >> (bitIndex % this.bitmapBlockSize))) != 0; // 128 decimals = 10000000 binary
            } else {
                return false;
            }
        }

        private int getBitMapSize(byte[] data) throws CopyBookException {
            // Read bit map size by comparing the last bit in each block
            int e;
            for(e = 1; (data[e * 8 - 1] & 1) != 0; ++e) { }
            int bitMapSize = e * this.bitmapBlockSize;
            if(bitMapSize > this.bitmapMaxSize) {
                throw new CopyBookException("Bitmap is to large for this copybook");
            }

            return bitMapSize;
        }
    }
}
