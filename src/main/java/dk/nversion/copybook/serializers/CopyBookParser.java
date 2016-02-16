/*
 * Copyright (c) 2015. Troels Liebe Bentsen <tlb@nversion.dk>
 * Licensed under the MIT license (LICENSE.txt)
 */

package dk.nversion.copybook.serializers;

import dk.nversion.copybook.annotations.*;
import dk.nversion.copybook.converters.TypeConverter;
import dk.nversion.copybook.converters.TypeConverterConfig;
import dk.nversion.copybook.exceptions.CopyBookException;
import dk.nversion.copybook.exceptions.TypeConverterException;

import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.nio.charset.Charset;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collector;
import java.util.stream.Collectors;

// TODO: Add packed decimal - http://www.simotime.com/datapk01.htm
// TODO: Add default VALUE after PIC

public class CopyBookParser {
    private static Pattern re_CopyBookLine = Pattern.compile("^\\s*(\\d+)\\s+([^\\s]+)((?:\\s+OCCURS\\s+\\d+(?:\\s+TO\\s+\\d+)?\\s+TIMES)|(?:\\s+REDEFINES\\s+[^\\s]+))?(\\s+PIC\\s+[^\\s]+)?(\\s+DEPENDING\\s+ON\\s+[^\\s]+(?:\\s+IN\\s+[^\\s+]+)?)?\\s*\\.\\s*$");
    private static Pattern re_Occurs = Pattern.compile("^\\s*OCCURS\\s+(?:(\\d+)\\s+TO\\s+)?(\\d+)\\s+TIMES\\s*$");
    private static Pattern re_Redefines = Pattern.compile("\\s*REDEFINES\\s+([^\\s]+)\\s*");
    private static Pattern re_Pic = Pattern.compile("^\\s*PIC\\s+(S)?(X+|9+)(?:\\((\\d+)\\))?(?:V(9+)(?:\\((\\d+)\\))?)?\\s*$");
    private static Pattern re_DependingOn = Pattern.compile("^\\s*DEPENDING\\s+ON\\s+([^\\s]+)(?:\\s+IN\\s+([^\\s]+))?\\s*$");

    private Class<? extends CopyBookMapper> serializerClass = null;
    private CopyBookSerializerConfig config = new CopyBookSerializerConfig();

    public CopyBookParser(Class<?> type) {
        this(type, false);
    }

    public CopyBookParser(Class<?> type, boolean debug) {
        // Read copybook annotations and defaults
        List<CopyBook> copybookAnnotations = getAnnotationsRecursively(CopyBookDefaults.class, CopyBook.class);
        copybookAnnotations.addAll(getAnnotationsRecursively(type, CopyBook.class));
        for(CopyBook annotation : copybookAnnotations) {
            if (!annotation.type().equals(CopyBookMapper.class)) {
                this.serializerClass = annotation.type();
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

        config.setDebug(debug);

        Map<String,TypeConverter> defaultTypeConverterMap = getTypeConvertersRecursively(CopyBookDefaults.class, config.getCharset());
        config.setFields(walkClass(type, null, defaultTypeConverterMap, config.getCharset(), new HashMap<>()));
    }

    @SuppressWarnings("unchecked")
    private List<CopyBookField> walkClass(Class<?> type, String copyBookName, Map<String,TypeConverter> inheritedTypeConverterMap, Charset charset, Map<String,CopyBookField> copyBookFieldNames) {
        List<CopyBookField> results = new ArrayList<>();

        CopyBook copyBookAnnotation = type.getAnnotation(CopyBook.class);
        if(copyBookAnnotation == null) {
            throw new CopyBookException("No copybook defined on this class");
        }

        // Overwrite type converts with what we find on the sub copybook
        Map<String,TypeConverter> typeConverterMap = new HashMap<>(inheritedTypeConverterMap);
        typeConverterMap.putAll(getTypeConvertersRecursively(type, charset));

        // Iterate over the class fields with CopyBookLine annotation
        for (Field field : type.getDeclaredFields()) {
            String[] copyBookLines = Arrays.stream(field.getAnnotationsByType(CopyBookLine.class)).map(cbl -> cbl.value()).toArray(String[]::new);
            CopyBookRedefine copyBookRedefine = field.getDeclaredAnnotation(CopyBookRedefine.class);
            String redefineOn = copyBookRedefine != null ? copyBookRedefine.on() : null;
            String redefineMatch = copyBookRedefine != null ? copyBookRedefine.match() : null;

            if(copyBookLines.length > 0) {
                String fieldName = type.getName() + "." + field.getName();
                Class<?> fieldBaseType = field.getType().isArray() ? field.getType().getComponentType() : field.getType();
                String fieldTypeName = getTypeClassSimpleName(fieldBaseType);
                List<String> names = new ArrayList<>();
                if(copyBookName != null) {
                    names.add(copyBookName);
                }
                int size = 0;
                int decimals = -1;
                int minOccurs = -1;
                int maxOccurs = -1;
                String counterKey = null;
                String copyBookType = null;
                String redefines = null;

                for(String copyBookLine : copyBookLines) {
                    Matcher copyBookLineMatcher = re_CopyBookLine.matcher(copyBookLine);
                    if (copyBookLineMatcher.find()) {
                        int level = Integer.parseInt(copyBookLineMatcher.group(1));
                        names.add(copyBookLineMatcher.group(2));

                        if (copyBookLineMatcher.group(3) != null) {
                            Matcher occursMatcher = re_Occurs.matcher(copyBookLineMatcher.group(3));
                            Matcher redefinesMatcher = re_Redefines.matcher(copyBookLineMatcher.group(3));
                            if (occursMatcher.find()) {
                                maxOccurs = Integer.parseInt(occursMatcher.group(2));
                                minOccurs = occursMatcher.group(1) != null ? Integer.parseInt(occursMatcher.group(1)) : maxOccurs;

                            } else if(redefinesMatcher.find()) {
                                redefines = redefinesMatcher.group(1);

                            } else {
                                throw new CopyBookException("Could not parse occurs section in copybook line for field '" + fieldName + "'");
                            }
                        }

                        if (copyBookLineMatcher.group(4) != null) {
                            Matcher picMatcher = re_Pic.matcher(copyBookLineMatcher.group(4));
                            if (picMatcher.find()) {
                                boolean signed = picMatcher.group(1) != null;
                                String mainType = picMatcher.group(2);
                                int mainSize = picMatcher.group(3) != null ? Integer.parseInt(picMatcher.group(3)) : mainType.length();
                                String decimalType = picMatcher.group(4) != null ? picMatcher.group(4) : "";
                                int decimalSize = picMatcher.group(5) != null ? Integer.parseInt(picMatcher.group(5)) : decimalType.length();

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
                                    if (signed) {
                                        copyBookType = "Signed" + copyBookType;
                                    }

                                } else {
                                    throw new CopyBookException("Unknown PIC type for field '" + fieldName + "'");
                                }


                            } else {
                                throw new CopyBookException("Could not parse occurs section in copybook line for field '" + fieldName + "'");
                            }
                        }

                        if (copyBookLineMatcher.group(5) != null) {
                            Matcher dependingOnMatcher = re_DependingOn.matcher(copyBookLineMatcher.group(5));
                            if (dependingOnMatcher.find()) {
                                String dependedName = dependingOnMatcher.group(1);
                                String subFieldName = dependingOnMatcher.group(2);

                                List<String> counterKeyNames = new ArrayList<>(names.subList(0, names.size() - 1));

                                if(subFieldName != null) {
                                    if(names.size() > 1) {
                                        // Go up one level and add subfield that the counter exists in
                                        counterKeyNames.remove(counterKeyNames.size() - 1);
                                        counterKeyNames.add(subFieldName);

                                    } else {
                                        throw new CopyBookException("IN only makes sense when you are in another level for field '" + fieldName + "'");
                                    }
                                }

                                counterKeyNames.add(dependedName);
                                counterKey = counterKeyNames.stream().collect(Collectors.joining("."));

                                if(copyBookFieldNames.containsKey(counterKey)) {
                                    copyBookFieldNames.get(counterKey).setCounter(true);

                                } else {
                                    throw new CopyBookException("Could not find referenced counter " + counterKey +  "for field '" + fieldName + "'");
                                }

                            } else {
                                throw new CopyBookException("Could not parse depending on section in copybook line for field '" + fieldName + "'");
                            }
                        }

                    } else {
                        throw new CopyBookException("Could not parse copybook line for field '" + fieldName + "'");
                    }
                }

                if((minOccurs > 0 || maxOccurs > 0) && !field.getType().isArray()) {
                    throw new CopyBookException("Trying to map array type to non array type for field '" + fieldName + "'");
                }

                // Find type converts that have been set on the field
                Map<String,TypeConverter> fieldTypeConverterMap = new HashMap<>(typeConverterMap);
                fieldTypeConverterMap.putAll(getTypeConvertersRecursively(fieldBaseType, charset));

                // Resolve typeConverter
                TypeConverter typeConverter = null;
                if(copyBookType != null) {
                    if (field.getType().isArray()) {
                        //TODO: Add ArrayTo example and test
                        typeConverter = fieldTypeConverterMap.get(copyBookType + "ArrayTo" + fieldTypeName + "Array");
                    }
                    if (typeConverter == null) {
                        typeConverter = fieldTypeConverterMap.get(copyBookType + "To" + fieldTypeName);
                    }
                    // Try to see if we have a convert for any of the interfaces this type implements
                    if (typeConverter == null) {
                        for (Class<?> aInterface : fieldBaseType.getInterfaces()) {
                            typeConverter = fieldTypeConverterMap.get(copyBookType + "To" + aInterface.getSimpleName());
                            if(typeConverter != null) {
                                try {
                                    typeConverter = typeConverter.copy(type);

                                } catch (IllegalAccessException | InstantiationException e) {
                                    throw new CopyBookException("Failed to copy type convert for this field '" + fieldName + "'", e);
                                }
                            }
                        }
                    }

                } else {
                    if (field.getType().isArray()) {
                        //TODO: Add ArrayTo example and test
                        typeConverter = fieldTypeConverterMap.get("ArrayTo" + fieldTypeName + "Array");
                    }
                    if (typeConverter == null) {
                        typeConverter = fieldTypeConverterMap.get("To" + fieldTypeName);
                    }
                }

                if(typeConverter != null) {
                    try {
                        typeConverter.validate(fieldBaseType, size, decimals);

                    } catch (TypeConverterException ex) {
                        throw new CopyBookException(fieldName + ":", ex);
                    }
                }

                String name = names.stream().collect(Collectors.joining("."));
                CopyBookField copyBookField = new CopyBookField(type, field, name, size, decimals, minOccurs, maxOccurs, copyBookLines, counterKey, typeConverter);
                copyBookFieldNames.put(name, copyBookField);
                copyBookField.setRedefines(getFullFieldName(names,redefines));
                copyBookField.setRedefinedOn(getFullFieldName(names, redefineOn));
                copyBookField.setRedefineMatch(redefineMatch);

                // Did not find a type convert so lets see if it's another copybook class
                if(typeConverter == null) {
                    if (fieldBaseType.getAnnotation(CopyBook.class) != null) {
                        copyBookField.setSubCopyBookFields(walkClass(fieldBaseType, name, typeConverterMap, charset, copyBookFieldNames));

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

    private String getTypeClassSimpleName(Class<?> type) {
        switch (type.getName()) {
            case "boolean": return "Boolean";
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

    @SuppressWarnings("unchecked")
    private <T extends Annotation> List<T> getAnnotationsRecursively(Class<?> type, Class<T> annotationType) {
        List<T> results = new ArrayList<>();
        for (Annotation annotation : type.getAnnotations()) {
            if(annotationType.isInstance(annotation)) {
                results.add((T)annotation);

            } else if (!annotation.annotationType().getName().startsWith("java")) {
                results.addAll(getAnnotationsRecursively(annotation.annotationType(), annotationType));
            }
        }
        return results;
    }

    private Map<String, TypeConverter> getTypeConvertersRecursively(Class<?> type, Charset charset) {
        Map<String,TypeConverter> results = new HashMap<>();
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

    private TypeConverter createTypeConverter(CopyBookFieldFormat copyBookFieldFormat, Charset charset) {
        TypeConverterConfig config = new TypeConverterConfig();
        config.setCharset(charset);
        config.setRightPadding(copyBookFieldFormat.rightPadding());
        config.setNullFillerChar(copyBookFieldFormat.nullFillerChar());
        config.setPaddingChar(copyBookFieldFormat.paddingChar());
        config.setSigningType(copyBookFieldFormat.signingType());
        config.setDefaultValue(copyBookFieldFormat.defaultValue().isEmpty() ? null : copyBookFieldFormat.defaultValue());
        config.setFormat(copyBookFieldFormat.format());

        try {
            TypeConverter typeConverter = copyBookFieldFormat.type().newInstance();
            typeConverter.initialize(config);
            return typeConverter;

        } catch (InstantiationException | IllegalAccessException ex) {
            throw new CopyBookException("Failed to load TypeConverterBase");

        } catch (TypeConverterException e) {
            throw new CopyBookException("Failed to initialize type convert", e);
        }
    }

    private String getFullFieldName(List<String> names, String name) {
        List<String> baseNames = new ArrayList<>(names.subList(0, names.size() - 1));
        baseNames.add(name);
        return baseNames.stream().collect(Collectors.joining("."));
    }

    public CopyBookSerializerConfig getConfig() {
        return config;
    }

    public void setConfig(CopyBookSerializerConfig config) {
        this.config = config;
    }

    public Class<? extends CopyBookMapper> getSerializerClass() {
        return serializerClass;
    }

    public void setSerializerClass(Class<? extends CopyBookMapper> serializerClass) {
        this.serializerClass = serializerClass;
    }

}
