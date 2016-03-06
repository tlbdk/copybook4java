package dk.nversion.copybook;

import dk.nversion.ByteUtils;
import dk.nversion.JavaSyntaxChecker;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.*;

import static org.junit.Assert.*;

public class CopyBookConverterTest {

    @org.junit.Test
    public void testCopyBookConverterTest() throws Exception {
        CopyBookConverter converter = new CopyBookConverter();
        InputStream sampleCopyBook = this.getClass().getResourceAsStream("../../../hospital.txt");
        String convertedJavaSource = converter.convert(sampleCopyBook, "mypackage", "MyHospital", "none", "UTF-8");

        File annotationsFolder = new File(this.getClass().getResource(".").getFile() + "../../../../../../copybook4java/src/main/java/dk/nversion/copybook/annotations");

        List<String> sources = new ArrayList<>();
        for(File file : annotationsFolder.listFiles()) {
            if(file.isFile() && file.getName().endsWith(".java")) {
                sources.add(new String(ByteUtils.toByteArray(new FileInputStream(file)), StandardCharsets.UTF_8));
            }
        }
        sources.add(convertedJavaSource);

        List<String> errors = JavaSyntaxChecker.check(sources);
        assertEquals(0, errors.size());
    }
}