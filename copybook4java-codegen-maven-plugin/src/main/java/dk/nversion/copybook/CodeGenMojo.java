package dk.nversion.copybook;

import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProject;

import java.io.File;
import java.io.InputStream;
import java.util.ServiceLoader;

@Mojo(name = "generate", defaultPhase = LifecyclePhase.GENERATE_SOURCES )
public class CodeGenMojo {

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
    private String inputCopyBook;

    /**
     * Client language to generate.
     *
     */
    @Parameter(required = true)
    private String language;

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
            CopyBookConverter converter = new CopyBookConverter();
            String convertedJavaSource = converter.convert(inputCopyBook);
            // FIXME: Save to disk

        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
