package com.zlatozar;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.concurrent.LinkedBlockingDeque;

/**
 * Here is the idea:
 * Create a buffer(FIFO) as wide as step ahead. In this way first and last could be interpreted (j = i + ahead).
 * In next step I remove first one and add last.
 * <p/>
 * ATTENTION: I assume that Scanner in FileLineIterator is lazy and returns next line on demand. If this is true
 * solution will be optimal.
 *
 * @author zlatozar@gmail.com
 */
public class ElegantSolution {

    protected int _ahead;

    private final LinkedBlockingDeque<List<Integer>> _slice;

    private FileLineIterator _iterator;

    public ElegantSolution(final int ahead) {

        if (ahead < 0) {
            throw new IllegalArgumentException("Step ahead should be positive");
        }

        this._ahead = ahead;
        this._slice = new LinkedBlockingDeque<List<Integer>>(ahead + 1);
    }

    public int find(final File file) throws FileNotFoundException {
        _iterator = new FileLineIterator(file);

        // special case
        if (_ahead == 0) {
            return find_special(file);
        }

        // init slice
        while (_iterator.hasNext() && _slice.size() <= _ahead) {
            _slice.add(_iterator.next());
        }

        if (_slice.size() < _ahead) {
            throw new IllegalStateException("There is no valid (i, j) pair. Please correct step ahead.");
        }

        int max = 0;
        int currMax;
        int countValid = 0;

        while (true) {

            // get first and remove it
            final List<Integer> valueI = _slice.pollFirst();

            // get but do not remove
            final List<Integer> valueJ = _slice.peekLast();

            if (valueI.size() > 1 && valueJ.size() > 1) {

                currMax = valueI.get(1) + valueJ.get(1);
                countValid++;

                if (countValid == 1 || currMax > max) {
                    max = currMax;
                }
            }

            try {
                _slice.add(_iterator.next());

            } catch (NoSuchElementException е) {
                break;
            }
        }

        // Hmm may be there is no solution
        if (countValid == 0) {
            throw new IllegalStateException("There is no valid (i, j) pair. Please correct step ahead.");
        }

        return max;
    }

    // Helper functions

    private int find_special(final File file) throws FileNotFoundException {

        _iterator = new FileLineIterator(file);

        int max = 0;
        int currMax;
        int countValid = 0;

        while (_iterator.hasNext()) {

            final List<Integer> metaInfo = _iterator.next();

            if (metaInfo.size() > 1) {
                currMax = metaInfo.get(1) + metaInfo.get(1);
                countValid++;

                if (countValid == 1 || currMax > max) {
                    max = currMax;
                }
            }
        }

        // Hmm may be there is no solution
        if (countValid == 0) {
            throw new IllegalStateException("There is no valid (i, j) pair. Please correct step ahead.");
        }

        return max;
    }

}
