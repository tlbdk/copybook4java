package dk.nversion.copybook.codegen;

import org.apache.maven.execution.DefaultMavenExecutionRequest;
import org.apache.maven.execution.MavenExecutionRequest;
import org.apache.maven.plugin.Mojo;
import org.apache.maven.plugin.testing.AbstractMojoTestCase;
import org.apache.maven.project.MavenProject;
import org.apache.maven.project.ProjectBuilder;
import org.apache.maven.project.ProjectBuildingRequest;
import org.eclipse.aether.DefaultRepositorySystemSession;
import org.junit.Test;

import java.io.File;

public class CodeGenMojoTest extends AbstractMojoTestCase {
    static {
        //System.setProperty("basedir", getBaseDir().getAbsolutePath());
    }

    // http://stackoverflow.com/questions/2146580/how-to-programmatically-call-a-maven-task

    @Test
    public void testExecute() throws Exception {
        File pom = getTestFile( "../copybook4java-codegen-test/pom.xml" );
        assertNotNull(pom);
        assertTrue(pom.exists());
        CodeGenMojo codeGenMojo = (CodeGenMojo)lookupConfiguredMojo("generate", pom);
        codeGenMojo.execute();
    }

    // http://www.programcreek.com/java-api-examples/index.php?source_dir=derby-maven-plugin-master/src/test/java/org/carlspring/maven/derby/AbstractDerbyMojoTest.java
    private Mojo lookupConfiguredMojo(String goal, File pom) throws Exception
    {
        MavenExecutionRequest request = new DefaultMavenExecutionRequest();
        request.setBaseDirectory(pom.getParentFile());
        ProjectBuildingRequest configuration = request.getProjectBuildingRequest();
        // Fix for bug: https://git-wip-us.apache.org/repos/asf?p=maven-plugin-testing.git;a=commit;h=3cd5f47c586499e438a3f9393304ac9d1f9a7f53
        configuration.setRepositorySession(new DefaultRepositorySystemSession());
        MavenProject project = lookup(ProjectBuilder.class).build(pom, configuration).getProject();
        return super.lookupConfiguredMojo(project, goal);
    }
}