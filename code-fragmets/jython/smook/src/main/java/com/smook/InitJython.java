package com.smook;

import jline.ConsoleReader;

import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import javax.script.ScriptException;
import java.io.IOException;
import java.io.InputStreamReader;

/**
 * The enter point of all the jython install madness could be solved here.
 * <p/>
 * So we want:
 * <p/>
 * Java initialization class (this) --&gt; Jython/Python installation --&gt;
 * Python script --&gt; uses some Java code
 *
 * @author Zlatozar Zhelyazkov <zlatozar@gmail.com>
 */
public class InitJython extends AbstractJythonInit {

    public InitJython(String[] args) {
        super(args);
    }

    public static void main(String[] args) throws ScriptException {
        System.out.println("1. Java started");

        new InitJython(args).run();

        System.out.println("\nJava exiting. Bye!");
    }

    public void run() throws ScriptException {

        System.out.print("Number of passed arguments (" + args.length + "). Arguments names: ");

        for (String s : args) {
            System.out.print(s);
            System.out.print(", ");
        }

        System.out.println();

        if (args.length > 0) {
            if (args[0].equals("eval")) {

                if (args.length > 1) {
                    console.exec(args[1]);

                } else {
                    console.exec("try:\n import examples\n examples.main()\nexcept SystemExit: pass");

                }

            } else if (args[0].equals("run")) {

                if (args.length > 1) {
                    console.execfile(args[1]);

                } else {
                    console.execfile(InitJython.class
                            .getResourceAsStream("Lib/examples/__init__.py"),
                                "examples/__init__.py");
                }

            } else if (args[0].equals("script")) {

                final String engineName = args[1];

                final ScriptEngine eng = new ScriptEngineManager().getEngineByName(engineName);

                if (eng == null) {
                    throw new NullPointerException("Script Engine '" + engineName + "' not found!");
                }

                eng.put("engine", engineName);

                if (args.length > 2) {
                    System.out.println("result: " + eng.eval(args[2]));

                } else {
                    System.out.println("Write your script below; terminate "
                            + "with Ctrl-Z (Windows) or Ctrl-D (Unix) ---");

                    try {
                        System.out.println("Result: "
                                + eng.eval(new InputStreamReader(
                                new ConsoleReader().getInput())));

                    } catch (IOException e) {
                        e.printStackTrace();
                    }
                }
            } else {
                System.out.println("Use either 'eval' or 'run' or " +
                        "script as first argument");
            }
        } else {
            console.interact();
        }
    }
}
