package dk.nversion.copybook.serializers;

import dk.nversion.ByteUtils;
import dk.nversion.copybook.exceptions.CopyBookException;

import java.lang.reflect.Array;
import java.nio.ByteBuffer;
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
                if(field.hasSubCopyBookFields()) {
                    // Complex array types fx. Request[]
                    if(field.getLevel() == 0) {
                        for(int j = 0; j < field.getMaxOccurs(); j++) {
                            Object item = field.getObject(rootObj, j);
                            if(item != null) {
                                writeFields(buffer, field.getSubCopyBookFields(), item, field.getMaxOccurs() - 1 == j);

                            } else {
                                buffer.put(null, true); // write separator
                            }
                        }

                    } else {
                        for(int j = 0; j < field.getMaxOccurs(); j++) {
                            writeFields(buffer, field.getSubCopyBookFields(), field.getObject(rootObj, j), last && field.getMaxOccurs() - 1 == j);
                        }
                    }

                } else {
                    // Simple array types, fx. int[]
                    Object array = field.getObject(rootObj);
                    for(int j = 0; j < field.getMaxOccurs(); j++) {
                        // Simple type fx. int, String or types we support with TypeConverters
                        if(field.getLevel() == 0) {
                            buffer.put(field.getBytes(rootObj, array, j, false), true); // No padding with separator

                        } else if(last) {
                            byte[] valueBytes = field.getBytes(rootObj, array, j, false); // Don't pad if it's the last field
                            if(valueBytes == null) {
                                valueBytes = field.getBytes(rootObj, array, j, true); // Except if it's null
                            }
                            buffer.put(valueBytes, true); // With separator

                        } else {
                            buffer.put(field.getBytes(rootObj, array, j, true), false); // With padding without separator
                        }
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
                if(field.getLevel() == 0) {
                    buffer.put(field.getBytes(rootObj, false), true); // No padding with separator

                } else if(last) {
                    byte[] valueBytes = field.getBytes(rootObj, false); // Don't pad if it's the last field
                    if(valueBytes == null) {
                        valueBytes = field.getBytes(rootObj, true); // Except if it's null
                    }
                    buffer.put(valueBytes, true); // With separator

                } else {
                    buffer.put(field.getBytes(rootObj, true), false); // With padding without separator
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
                System.out.println("'" + new String(bytes) +"'");

                if(separator) {
                    setBitInBitmap();
                    this.maxUsedBit = bitmapIndex;
                    buffer.put(this.separatorByte);
                }
            }
            if(separator) {
                System.out.println("-----------");
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
