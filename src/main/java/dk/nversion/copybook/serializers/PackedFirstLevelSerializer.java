package dk.nversion.copybook.serializers;

import dk.nversion.ByteUtils;
import dk.nversion.copybook.exceptions.CopyBookException;

import java.lang.reflect.Array;
import java.nio.ByteBuffer;
import java.util.List;

public class PackedFirstLevelSerializer extends CopyBookSerializerBase {
    private int maxRecordSize;
    private int maxBitmapSize;
    private byte separatorByte;
    private int bitmapBlockSize;

    public PackedFirstLevelSerializer(CopyBookSerializerConfig config) {
        super(config);
        this.bitmapBlockSize =  8;//config.getBitmapBlockSize();
        int packingItemsCount = countPackingItems(config.getFields());
        int maxBitmapBlocks = packingItemsCount / (bitmapBlockSize * 8 - 1) + 1;
        this.maxBitmapSize = bitmapBlockSize * maxBitmapBlocks;
        this.maxRecordSize = calculateMaxSize(config.getFields(), 0, this.debug) + packingItemsCount; // Separator bytes to count
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

        for(int i = 0; i < fields.size(); i++) {
            CopyBookField field = fields.get(i);
            boolean lastField = fields.size() - 1 == i;
            if(field.isArray()) {
                if(field.hasSubCopyBookFields()) {
                    // Complex array types fx. Request[]
                    for(int j = 0; j < field.getMaxOccurs(); j++) {
                        Object item = field.getObject(obj, j);
                        if(item != null) {
                            buffer.put(field.getSubCopyBookFields(), item, 0, lastField);

                        } else {
                            buffer.put();
                        }
                    }

                } else {
                    // Simple array types, fx. int[]
                    for(int j = 0; j < field.getMaxOccurs(); j++) {
                        buffer.put(field.getBytes(obj, j, false));
                    }
                }

            } else if(field.hasSubCopyBookFields()) {
                Object item = field.getObject(obj);
                if(item != null) {
                    buffer.put(field.getSubCopyBookFields(), item, 0, lastField);

                } else {
                    buffer.put();
                }

            } else {
                buffer.put(field.getBytes(obj, false));
            }
        }

        return buffer.array();
    }

    private class PackedBuffer {
        private ByteBuffer buffer;
        private byte[] bitmapBytes;
        private int bitmapIndex = 0;
        private int maxUsedBit = 0;
        private int bitmapBlockSize;
        private byte separatorByte;

        public PackedBuffer(int size, int bitmapSize, int bitmapBlockSize, byte separatorByte) {
            this.buffer = ByteBuffer.wrap(new byte[size]);
            this.bitmapBytes = new byte[bitmapSize];
            this.bitmapBlockSize = bitmapBlockSize;
            this.separatorByte = separatorByte;
        }

        public void put() throws CopyBookException {
            put(null, true);
        }

        public void put(byte [] bytes) throws CopyBookException {
            put(bytes, true);
        }

        public void put(byte [] bytes, boolean separator) throws CopyBookException {
            if(bytes != null) {
                if (ByteUtils.indexOf(bytes, this.separatorByte, 0, bytes.length) > -1) {
                    throw new CopyBookException("Bytes contains the separator char");
                }
                buffer.put(bytes);

                if(separator) {
                    setBitInBitmap();
                    this.maxUsedBit = bitmapIndex;
                    buffer.put(this.separatorByte);
                }
            }
            if(separator) {
                this.bitmapIndex++;
            }
        }

        public void put(List<CopyBookField> fields, Object obj) throws CopyBookException {
            put(fields, obj, 0, false);
        }

        public void put(List<CopyBookField> fields, Object obj, int level, boolean last) throws CopyBookException {
            for (int i = 0; i < fields.size(); i++) {
                CopyBookField field = fields.get(i);
                boolean lastField = fields.size() - 1 == i;
                if (field.isArray()) {
                    if (field.hasSubCopyBookFields()) {
                        // Complex array types fx. Request[]
                        for (int j = 0; j < field.getMaxOccurs(); j++) {
                            put(field.getSubCopyBookFields(), field.getObject(obj, j), level + 1, last && lastField && field.getMaxOccurs() -1 == j);
                        }

                    } else {
                        // Simple array types, fx. int[]
                        for (int j = 0; j < field.getMaxOccurs(); j++) {
                            if(last && lastField && field.getMaxOccurs() - 1 == j) { // Last field in root object
                                byte[] valueBytes = field.getBytes(obj, i, false); // Don't pad if it's the last field
                                if(valueBytes == null) {
                                    valueBytes = field.getBytes(obj, i, true); // Except if it's null
                                }
                                put(valueBytes);

                            } else {
                                put(field.getBytes(obj, i, true), false);
                            }
                        }
                    }

                } else if(field.hasSubCopyBookFields()) {
                    // Complex type fx, Request
                    put(field.getSubCopyBookFields(), field.getObject(obj), level + 1, lastField && last);

                } else {
                    // Simple type fx. int, String or types we support with TypeConverters
                    put(field.getBytes(obj, true), false);
                }
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
    }


    @Override
    public <T> T deserialize(byte[] bytes, Class<T> type) throws CopyBookException, InstantiationException {
        return null;
    }



    private boolean getBitInBitmap(byte[] bytes, int bitIndex, int bitmapSize) {
        bitIndex += bitIndex / 63;
        if(bitIndex < bitmapSize * 8) {
            return (bytes[bitIndex / this.bitmapBlockSize] & (128 >> (bitIndex % this.bitmapBlockSize))) != 0; // 128 decimals = 10000000 binary
        } else {
            return false;
        }
    }

}
