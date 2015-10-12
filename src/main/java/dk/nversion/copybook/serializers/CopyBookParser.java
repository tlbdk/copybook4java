package dk.nversion.copybook.serializers;

import dk.nversion.copybook.CopyBookException;
import dk.nversion.copybook.annotations.*;
import dk.nversion.copybook.converters.TypeConverterBase;
import dk.nversion.copybook.converters.TypeConverterConfig;

import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.nio.charset.Charset;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class CopyBookParser {
    private Pattern re_simpletype = Pattern.compile("^\\s*(\\d+)\\s+([^\\s\\.]+)\\.\\s*$");
    private Pattern re_piconly = Pattern.compile("^\\s*(\\d+)\\s+([^\\s]+)\\s+PIC\\s+(S)?(X+|9+)(?:\\((\\d+)\\))?(?:V(9+)(?:\\((\\d+)\\))?)?\\s*\\.\\s*$");
    private Pattern re_occursMax = Pattern.compile("^\\s*(\\d+)\\s+([^\\s]+)\\s+OCCURS\\s+(\\d+)\\s+TIMES\\s*\\.$");
    private Pattern re_occursAndPic = Pattern.compile("^\\s*(\\d+)\\s+([^\\s]+)\\s+OCCURS\\s+(\\d+)\\s+TIMES\\s+PIC\\s+(S)?(X+|9+)(?:\\((\\d+)\\))?(?:V(9+)(?:\\((\\d+)\\))?)?\\s*\\.\\s*$");

    private Class<? extends CopyBookSerializerBase> serializerClass = null;
    private CopyBookSerializerConfig config = new CopyBookSerializerConfig();

    public CopyBookParser(Class type) throws CopyBookException {
        // Read copybook annotations and defaults
        List<CopyBook> copybookAnnotations = getAnnotationsRecursively(CopyBookDefaults.class, CopyBook.class);
        copybookAnnotations.addAll(getAnnotationsRecursively(type, CopyBook.class));
        for(CopyBook annotation : copybookAnnotations) {
            if (!annotation.type().equals(CopyBookSerializerBase.class)) {
                serializerClass = annotation.type();
            }
            if (!annotation.charset().isEmpty()) {
                config.setCharset(Charset.forName(annotation.charset()));
            }
            if(annotation.separatorChar() != 'G') { // Java sucks and does not support null as default value for annotations so we pick large G as this is å in EBCDIC and unlikely to be used as separator char
                config.setSeparatorByte((byte)annotation.separatorChar());
            }
            if(annotation.bitmapBlockSize() != 0) {
                config.setBitmapBlockSize(annotation.bitmapBlockSize());
            }
        }

        if(serializerClass == null) {
            throw new CopyBookException("Serialization class missing");
        }

        Map<String,TypeConverterBase> defaultTypeConverterMap = getTypeConvertersRecursively(CopyBookDefaults.class, config.getCharset());
        config.setFields(walkClass(type, defaultTypeConverterMap, config.getCharset()));
    }

    private List<CopyBookField> walkClass(Class type, Map<String,TypeConverterBase> inheritedTypeConverterMap, Charset charset) throws CopyBookException {
        List<CopyBookField> results = new ArrayList<>();

        CopyBook copyBookAnnotation = (CopyBook)type.getAnnotation(CopyBook.class);
        if(copyBookAnnotation == null) {
            throw new CopyBookException("No copybook defined on this class");
        }

        // TODO: Verify that values from CopyBook annotation matches the root copybook else throw exception

        // Overwrite type converts with what we find on the sub copybook
        Map<String,TypeConverterBase> typeConverterMap = new HashMap<>(inheritedTypeConverterMap);
        typeConverterMap.putAll(getTypeConvertersRecursively(type, charset));

        // Iterate over the class fields with CopyBookLine annotation
        for (Field field : type.getDeclaredFields()) {
            String[] copyBookLines = Arrays.stream((CopyBookLine[])field.getAnnotationsByType(CopyBookLine.class)).map(cbl -> cbl.value()).toArray(String[]::new);
            if(copyBookLines.length > 0) {
                String fieldName = type.getName() + "." + field.getName();
                Class fieldBaseType = field.getType().isArray() ? field.getType().getComponentType() : field.getType();
                String fieldTypeName = getTypeClassSimpleName(fieldBaseType);
                String name = null;
                int size = 0;
                int decimals = -1;
                int minOccurs = -1;
                int maxOccurs = -1;
                String counterKey = null;
                String copyBookType = null;

                for(String copyBookLine : copyBookLines) {
                    // Parse copybook line and validate types
                    Matcher simpletype_matcher = re_simpletype.matcher(copyBookLine);
                    Matcher piconly_matcher = re_piconly.matcher(copyBookLine);
                    Matcher picAndOccurs_matcher = re_occursAndPic.matcher(copyBookLine);
                    Matcher occurs_matcher = re_occursMax.matcher(copyBookLine);

                    if (simpletype_matcher.find()) {
                        int level = Integer.parseInt(simpletype_matcher.group(1));
                        name = simpletype_matcher.group(2);

                    } else if(piconly_matcher.find()) {
                        int level = Integer.parseInt(piconly_matcher.group(1));
                        name = piconly_matcher.group(2);
                        boolean signed = piconly_matcher.group(3) != null;
                        String mainType = piconly_matcher.group(4);
                        int mainSize = piconly_matcher.group(5) != null ? Integer.parseInt(piconly_matcher.group(5)) : mainType.length();
                        String decimalType = piconly_matcher.group(6) != null ?  piconly_matcher.group(6) : "";
                        int decimalSize = piconly_matcher.group(7) != null ? Integer.parseInt(piconly_matcher.group(7)) : decimalType.length();

                        // Add decimals to size if they are set
                        size = mainSize + decimalSize;
                        decimals = decimalSize;

                        // Find type for this copybook line
                        if (mainType.startsWith("X")) { // String type
                            copyBookType = "String";

                        } else if (mainType.startsWith("9")) { // Signed number
                            if (!decimalType.isEmpty()) {
                               copyBookType = "Decimal";

                            } else {
                                copyBookType = "Integer";
                            }
                            if(signed) {
                                copyBookType = "Signed" + copyBookType;
                            }

                        } else {
                            throw new CopyBookException("Unknown PIC type for field '" + fieldName + "'");
                        }

                    } else if (picAndOccurs_matcher.find()) {
                        // TODO

                    } else if (occurs_matcher.find()) {
                        int level = Integer.parseInt(occurs_matcher.group(1));
                        name = occurs_matcher.group(2);
                        minOccurs = Integer.parseInt(occurs_matcher.group(3));
                        maxOccurs = minOccurs;

                    } else {
                        throw new CopyBookException("Could not parse copybook line for field '" + fieldName + "'");
                    }
                }

                if((minOccurs > 0 || maxOccurs > 0) && !field.getType().isArray()) {
                    throw new CopyBookException("Trying to map array type to non array type for field '" + fieldName + "'");
                }

                // Find type converts that have been set on the field
                Map<String,TypeConverterBase> fieldTypeConverterMap = new HashMap<>(typeConverterMap);
                fieldTypeConverterMap.putAll(getTypeConvertersRecursively(fieldBaseType, charset));

                // Resolve typeConverter
                TypeConverterBase typeConverter = null;
                if(copyBookType != null) {
                    if (field.getType().isArray()) {
                        typeConverter = fieldTypeConverterMap.get(copyBookType + "ArrayTo" + fieldTypeName + "Array");
                    }
                    if (typeConverter == null) {
                        typeConverter = fieldTypeConverterMap.get(copyBookType + "To" + fieldTypeName);
                    }

                } else {
                    if (field.getType().isArray()) {
                        typeConverter = fieldTypeConverterMap.get("ArrayTo" + fieldTypeName + "Array");
                    }
                    if (typeConverter == null) {
                        typeConverter = fieldTypeConverterMap.get("To" + fieldTypeName);
                    }
                }

                CopyBookField copyBookField = new CopyBookField(field, size, decimals, minOccurs, maxOccurs, copyBookLines, counterKey, typeConverter);

                // Did not find a type convert so lets see if it's another copybook class
                if(typeConverter == null) {
                    if (fieldBaseType.getAnnotation(CopyBook.class) != null) {
                        copyBookField.setSubCopyBookFields(walkClass(fieldBaseType, typeConverterMap, charset));

                    } else {
                        if(fieldBaseType.isPrimitive() || fieldBaseType.getName().startsWith("java")) {
                            if(copyBookType == null) {
                                throw new CopyBookException("Error mapping field '" + fieldName + "' as it is not defining a type in the copybook line");

                            } else {
                                throw new CopyBookException("Error mapping field '" + fieldName + "', could not find field converter defined for " + copyBookType + " to " + fieldTypeName);
                            }

                        } else {
                            if(copyBookType == null) {
                                throw new CopyBookException("CopyBook annotation not found on type for field '" + fieldName + "' or no field converter defined for " + fieldTypeName);

                            } else {
                                throw new CopyBookException("CopyBook annotation not found on type for field '" + fieldName + "' or no field converter defined for " + copyBookType + " to " + fieldTypeName);
                            }
                        }
                    }
                }

                results.add(copyBookField);
            }
        }

        return results;
    }

    private String getTypeClassSimpleName(Class type) {
        switch (type.getName()) {
            case "int": return "Integer";
            case "long": return "Long";
            case "double": return "Double";
            case "bool": return "Bool";
            case "char": return "Char";
            case "byte": return "Byte";
            case "void": return "Void";
            default: return type.getSimpleName();
        }
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

    private Map<String, TypeConverterBase> getTypeConvertersRecursively(Class type, Charset charset) throws CopyBookException {
        Map<String,TypeConverterBase> results = new HashMap<>();
        for (Annotation annotation : type.getAnnotations()) {
            if(CopyBookFieldFormats.class.isInstance(annotation)) {
                for(CopyBookFieldFormat fieldFormat : ((CopyBookFieldFormats)annotation).value()) {
                    results.put(fieldFormat.type().getSimpleName(), createTypeConverter(fieldFormat, charset));
                }

            } else if(CopyBookFieldFormat.class.isInstance(annotation)) {
                CopyBookFieldFormat fieldFormat = (CopyBookFieldFormat)annotation;
                results.put(fieldFormat.type().getSimpleName(), createTypeConverter(fieldFormat, charset));

            } else if (!annotation.annotationType().getName().startsWith("java")) {
                results.putAll(getTypeConvertersRecursively(annotation.annotationType(), charset));
            }
        }
        return results;
    }

    private TypeConverterBase createTypeConverter(CopyBookFieldFormat copyBookFieldFormat, Charset charset) throws CopyBookException {
        TypeConverterConfig config = new TypeConverterConfig();
        config.setCharset(charset);
        config.setRightPadding(copyBookFieldFormat.rightPadding());
        config.setNullFillerChar(copyBookFieldFormat.nullFillerChar());
        config.setPaddingChar(copyBookFieldFormat.paddingChar());
        config.setSigningType(copyBookFieldFormat.signingType());

        try {
            return (TypeConverterBase)copyBookFieldFormat.type().getDeclaredConstructor(TypeConverterConfig.class).newInstance(config);

        } catch (InstantiationException | IllegalAccessException | InvocationTargetException | NoSuchMethodException e) {
            throw new CopyBookException("Failed to load TypeConverterBase");
        }
    }

    public CopyBookSerializerConfig getConfig() {
        return config;
    }

    public void setConfig(CopyBookSerializerConfig config) {
        this.config = config;
    }

    public Class<? extends CopyBookSerializerBase> getSerializerClass() {
        return serializerClass;
    }

    public void setSerializerClass(Class<? extends CopyBookSerializerBase> serializerClass) {
        this.serializerClass = serializerClass;
    }

}