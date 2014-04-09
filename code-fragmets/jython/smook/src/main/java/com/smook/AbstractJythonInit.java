package com.smook;

import org.python.core.Py;
import org.python.core.PyFile;
import org.python.core.PySystemState;
import org.python.core.imp;
import org.python.util.InteractiveConsole;
import org.python.util.JLineConsole;

import java.util.Properties;

/**
 * Creates Jython environment.
 *
 * @author Zlatozar Zhelyazkov <zlatozar@gmail.com>
 */
public abstract class AbstractJythonInit {

    protected final String[] args;
    protected final InteractiveConsole console;

    public AbstractJythonInit(String[] args) {
        this.args = args;

        PySystemState.initialize(PySystemState.getBaseProperties(),
                new Properties(), args);

        // init console
        console = createInterpreter(checkIsInteractive());
    }

    protected boolean checkIsInteractive() {
        final PySystemState systemState = Py.getSystemState();
        final boolean interactive = ((PyFile) Py.defaultSystemState.stdin).isatty();

        // PS1 console variable
        if (!interactive) {
            systemState.ps1 = systemState.ps2 = Py.EmptyString;
        }

        return interactive;
    }

    protected InteractiveConsole createInterpreter(boolean interactive) {
        final InteractiveConsole interpreter = newInterpreter(interactive);
        Py.getSystemState().__setattr__("_jy_interpreter", Py.java2py(interpreter));

        imp.load("site");
        return interpreter;
    }

    // Helper functions

    private InteractiveConsole newInterpreter(boolean interactiveStdin) {
        if (!interactiveStdin) {
            return new InteractiveConsole();
        }

        final String interpClass = PySystemState.registry.getProperty(
                "python.console", "");

        if (interpClass.length() > 0) {
            try {

                return (InteractiveConsole) Class.forName(interpClass)
                        .newInstance();

            } catch (Throwable t) {
                // fall through
            }
        }

        return new JLineConsole();
    }
}
