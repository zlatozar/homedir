package com.zlatozar;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Scanner;

/**
 * Iterate over file that contains a sequence of integer numbers stored one per line.
 * <p/>
 * There might be lines that do not entirely represent integers, e.g. 2u1, 23.9, #12, etc.
 * Such lines are considered as comments, i.e they do not carry numbers.
 *
 * @author zlatozar@gmail.com
 */
public class FileLineIterator implements Iterator<List<Integer>> {

    private final Scanner _scanner;
    private Integer _currentLine = 0;

    public FileLineIterator(final File file) throws FileNotFoundException {
        this._scanner = new Scanner(file, "UTF-8");
    }

    @Override
    public boolean hasNext() {
        return _scanner.hasNextLine();
    }

    /**
     * @return Tuple with one or two values - (<line number>, <integer value>).
     * If list is with size one this means that value on this line is invalid.
     */
    @Override
    public List<Integer> next() {

        // (line, value(exist or not))
        final List<Integer> tuple = new ArrayList<Integer>(2);
        tuple.add(_currentLine);

        try {
            tuple.add(Integer.parseInt(_scanner.nextLine()));

        } catch (NumberFormatException e) {
            // not important

        } finally {
            _currentLine++;
        }

        return tuple;
    }

    @Override
    public void remove() {
        throw new UnsupportedOperationException("Not implemented");
    }

    // Helper functions

    public int getCurrentLineNumber() {
        return _currentLine;
    }

}
