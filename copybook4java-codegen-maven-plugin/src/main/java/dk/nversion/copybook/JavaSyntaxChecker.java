package dk.nversion;

import javax.tools.*;
import java.io.IOException;
import java.net.URI;
import java.nio.charset.StandardCharsets;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

// TODO: Fix storing of class files so it is done in memory instead in the root of the project folder

public class JavaSyntaxChecker {
    static JavaCompiler javac = ToolProvider.getSystemJavaCompiler();
    static Pattern re_packageName = Pattern.compile("^.*?package\\s*([^\\s;]+)", Pattern.DOTALL);
    static Pattern re_className = Pattern.compile("^.*?\\s(?:class|@interface|enum)\\s*([^\\s]+)", Pattern.DOTALL);

    public static List<String> check(List<String> sources) throws Exception {
        List<StringSourceCode> stringSourceCodes = new ArrayList<>();
        for(String source : sources) {
            String packageName = "";
            Matcher packageNameMatcher = re_packageName.matcher(source);
            if(packageNameMatcher.find()) {
                packageName = packageNameMatcher.group(1) + ".";
            }

            Matcher classNameMatcher = re_className.matcher(source);
            if (classNameMatcher.find()) {
                stringSourceCodes.add(new StringSourceCode(packageName + classNameMatcher.group(1), source));
            }
        }

        StandardJavaFileManager fileManager = javac.getStandardFileManager(null, null, StandardCharsets.UTF_8);
        DiagnosticCollector<JavaFileObject> diagnostics = new DiagnosticCollector<>();
        JavaCompiler.CompilationTask task = javac.getTask(null, null, diagnostics, null, null, stringSourceCodes);
        task.call();

        List<String> errors = new ArrayList<>();
        for (Diagnostic<? extends JavaFileObject> diagnostic : diagnostics.getDiagnostics()) {
            errors.add(diagnostic.getLineNumber() + ":" + diagnostic.getPosition() + " : " + diagnostic.getMessage(null) + " : " + diagnostic.getSource() );
        }

        return errors;
    }

    private static class StringSourceCode extends SimpleJavaFileObject {
        private String contents = null;

        public StringSourceCode(String className, String contents) throws Exception {
            super(URI.create("string:///" + className.replace('.', '/') + Kind.SOURCE.extension), Kind.SOURCE);
            this.contents = contents;
        }

        public CharSequence getCharContent(boolean ignoreEncodingErrors) throws IOException {
            return contents;
        }
    }
}
