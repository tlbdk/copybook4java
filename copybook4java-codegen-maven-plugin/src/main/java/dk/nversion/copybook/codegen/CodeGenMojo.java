package dk.nversion.copybook.codegen;

import dk.nversion.ByteUtils;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProject;

import java.io.File;
import java.io.FileInputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;
import java.util.regex.Pattern;

@Mojo(name = "generate", defaultPhase = LifecyclePhase.GENERATE_SOURCES )
public class CodeGenMojo extends AbstractMojo {

    /**
     * Location of the file.
     *
     */
    @Parameter(defaultValue = "${project.build.directory}", required = true)
    private File outputPath;

    /**
     * Location of the copybook definition, as URL or file. For directories this will recurse through directory tree
     * and use sub directory as part of the package name.
     *
     */
    @Parameter(required = true)
    private File inputPath;


    /**
     * Regex will be applied to all files found in the inputPath
     *
     */
    @Parameter(defaultValue = "^.*$")
    private String inputFilter;

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
     * Subclass type: none, nested
     *
     */
    @Parameter(defaultValue = "nested")
    private String subclass;

    /**
     * The outputPath package root name.
     */
    @Parameter(defaultValue = "mypackage")
    private String packageName;

    /**
     * Add the outputPath directory to the project as a source root, so that the
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
            CopyBookConverter converter = new CopyBookConverter();
            Path outPath = Paths.get(outputPath.getCanonicalPath(), "generated-sources");
            converter.convertFiles(inputPath.getCanonicalPath(), Pattern.compile(inputFilter), outPath.toString(), packageName, accessor, charset, subclass);

            // Make sure we add the generated code to Maven so it gets compiled
            if (addCompileSourceRoot) {
                project.addCompileSourceRoot(outputPath.toString());
            }

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
