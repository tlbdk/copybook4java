package dk.nversion.copybook;

import dk.nversion.ByteUtils;
import dk.nversion.copybook.codegen.CopyBookConverter;
import dk.nversion.copybook.codegen.JavaSyntaxChecker;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.*;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import static org.junit.Assert.*;

public class CopyBookConverterTest {

    @org.junit.Test
    public void testBasicConversionAndVerifyWithCompilation() throws Exception {
        CopyBookConverter converter = new CopyBookConverter();
        InputStream sampleCopyBook = this.getClass().getResourceAsStream("../../../hospital_test.txt");
        List<String> convertedJavaSources = converter.convert(sampleCopyBook, "mypackage", "MyHospital", "none", "UTF-8", "nested", null);

        // Add header to new source files
        /* String header = "package " + "renamemepackage" + ";\n\n"
                 + "import dk.nversion.copybook.annotations.CopyBook;\n"
                 + "import dk.nversion.copybook.annotations.CopyBookLine;\n\n";
        convertedJavaSources = convertedJavaSources.stream().map(s ->  header + s).collect(Collectors.toList()); */

        List<String> errors = CompileGeneratedSource(convertedJavaSources);
        assertEquals(0, errors.size());
    }

    @org.junit.Test
    public void testMultiFileConversion() throws Exception {
        File basePath = new File(this.getClass().getResource("../../../").toURI());

        CopyBookConverter converter = new CopyBookConverter();
        converter.convertFiles(basePath.getAbsolutePath(), Pattern.compile("\\.txt$"), basePath.getAbsolutePath(), "none", "UTF-8", "nested");
        //String convertedJavaSource = converter.convert(new File("../../../*.txt"), "mypackage", "MyHospital", "none", "UTF-8");
    }

    private List<String> CompileGeneratedSource(List<String> convertedJavaSources) throws Exception {
        File annotationsFolder = new File(this.getClass().getResource(".").getFile() + "../../../../../../copybook4java/src/main/java/dk/nversion/copybook/annotations");

        List<String> sources = new ArrayList<>();
        for(File file : annotationsFolder.listFiles()) {
            if(file.isFile() && file.getName().endsWith(".java")) {
                sources.add(new String(ByteUtils.toByteArray(new FileInputStream(file)), StandardCharsets.UTF_8));
            }
        }
        sources.addAll(convertedJavaSources);
        return JavaSyntaxChecker.check(sources);
    }

}