package dk.nversion.copybook;

import dk.nversion.ByteUtils;

import javax.script.Invocable;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import javax.script.ScriptException;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;

public class CopyBookConverter {
    private final ScriptEngineManager manager;
    private ScriptEngine engine;
    private final Invocable invocable;

    public interface JSProvider {
        String convertCopybook(String json, String accessor, String charset);
    }

    public CopyBookConverter() throws Exception {
        InputStream inputStream = this.getClass().getResourceAsStream("classconverter.html");
        String js = extractJS(inputStream);

        manager = new ScriptEngineManager();
        engine = manager.getEngineByName("nashorn");

        // Load Rhino if we fail to load nashorn
        if(engine == null) {
            engine = manager.getEngineByName("rhino");
        }
        if(engine == null) {
            throw new Exception("Could not load either the Nashorn or Rhino script engine");
        }

        invocable = (Invocable) engine;
        engine.eval(js);
    }

    public String convert(String copybook) throws ScriptException, NoSuchMethodException {
        return (String)invocable.invokeFunction("convertCopybook", "myPackage", "myCopyBook", copybook, "none", "UTF-8");
    }

    public String convert(InputStream copybook) throws ScriptException, NoSuchMethodException, IOException {
        return convert(new String(ByteUtils.toByteArray(copybook), StandardCharsets.UTF_8));
    }

    private String extractJS(InputStream inputStream) throws IOException {
        BufferedReader bufferedReader = new BufferedReader(new InputStreamReader(inputStream));
        StringBuilder builder = new StringBuilder();
        String str;
        boolean capture = false;
        int whiteSpaceOffset = 0;
        while ((str = bufferedReader.readLine()) != null) {
            if(str.indexOf("/*** convertCopybook stop ***/") > 0) {
                capture = false;
            }

            if(capture) {
                builder.append(str.substring(str.length() > whiteSpaceOffset ? whiteSpaceOffset : 0) + "\n");
            }

            if(str.indexOf("/*** convertCopybook start ***/") > 0) {
                while (whiteSpaceOffset < str.length() && Character.isWhitespace(str.charAt(whiteSpaceOffset))) {
                    whiteSpaceOffset++;
                }
                capture = true;
            }
        }
        return builder.toString();
    }
}
