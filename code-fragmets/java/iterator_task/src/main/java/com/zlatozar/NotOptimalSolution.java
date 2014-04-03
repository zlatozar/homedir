package com.zlatozar;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.List;

/**
 * This class represents my idea that I came up first.
 *
 * @author zlatozar@gmail.com
 */
public class NotOptimalSolution {

    protected int _ahead;

    private FileLineIterator _iterator;

    public NotOptimalSolution(final int ahead) {
        if (ahead < 0) {
            throw new IllegalArgumentException("Step ahead should be positive");
        }
        this._ahead = ahead;
    }

    /**
     * Finds the solution of the main question.
     * <p/>
     * NOTE: Everything is in one method because I would like to show the algorithm
     *
     * @return maximum sum between integers placed in lines (i, j)
     * j is found using formula j = i + ahead
     */
    public int find(final File file) throws FileNotFoundException {
        int max = 0;
        int currMax;

        _iterator = new FileLineIterator(file);

        // step one - find valid lines
        final List<List<Integer>> metaInfo = new ArrayList<List<Integer>>();
        while (_iterator.hasNext()) {
            // ATTENTION: Everything in memory....bad
            metaInfo.add(_iterator.next());
        }

        // step two - use formula j = i + ahead to calculate max sum
        int countValid = 0;
        for (int i = 0; i < metaInfo.size() - _ahead; i++) {

            if (metaInfo.get(i).size() > 1 && metaInfo.get(i + _ahead).size() > 1) {
                currMax = metaInfo.get(i).get(1) + metaInfo.get(i + _ahead).get(1);
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

    /**
     * Set new ahead value
     *
     * @param newAheadValue
     */
    public void setAhead(final int newAheadValue) {
        _ahead = newAheadValue;
    }
}



