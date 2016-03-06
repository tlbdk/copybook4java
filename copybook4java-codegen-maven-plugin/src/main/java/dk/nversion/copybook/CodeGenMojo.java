package dk.nversion.copybook;

import dk.nversion.ByteUtils;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProject;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.ServiceLoader;

@Mojo(name = "generate", defaultPhase = LifecyclePhase.GENERATE_SOURCES )
public class CodeGenMojo extends AbstractMojo {

    /**
     * Location of the file.
     *
     */
    @Parameter(defaultValue = "${project.build.directory}", required = true)
    private File output;

    /**
     * Location of the copybook definition, as URL or file.
     *
     */
    @Parameter(required = true)
    private File inputCopyBook;

    /**
     * Language to generate for
     *
     */
    @Parameter(defaultValue = "java")
    private String language;

    /**
     *  Charset
     *
     */
    @Parameter(defaultValue = "UTF-8")
    private String charset;

    /**
     * Accessor type: none, getset, lombok
     *
     */
    @Parameter(defaultValue = "none")
    private String accessor;

    /**
     * The output package name.
     */
    @Parameter(defaultValue = "mypackage")
    private String packageName;

    /**
     * Add the output directory to the project as a source root, so that the
     * generated java types are compiled and included in the project artifact.
     */
    @Parameter(defaultValue = "true")
    private boolean addCompileSourceRoot = true;

    /**
     * The project being built.
     */
    @Parameter(readonly = true, required = true, defaultValue = "${project}")
    private MavenProject project;

    public void execute() throws MojoExecutionException {
        try {
            String copybookString = new String(ByteUtils.toByteArray(new FileInputStream(inputCopyBook)), StandardCharsets.UTF_8);

            String className = getClassName(inputCopyBook);

            CopyBookConverter converter = new CopyBookConverter();
            String convertedJavaSource = converter.convert(copybookString, "mypackage", className, "none", "UTF-8");
            System.out.println("Hello");
            //Files.write(output)
            // FIXME: Save to disk

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private String getClassName(File file) {
        String name = file.getName();
        try {
            return Character.toUpperCase(name.charAt(0)) + name.substring(1, name.lastIndexOf("."));

        } catch (Exception e) {
            return "";
        }
    }

}
