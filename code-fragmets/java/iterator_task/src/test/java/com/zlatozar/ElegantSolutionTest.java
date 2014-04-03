package com.zlatozar;

import org.junit.Assert;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import java.io.File;
import java.net.URI;

/**
 * Tests for {@link com.zlatozar.ElegantSolution}
 *
 * @author zlatozar@gmail.com
 */
public class ElegantSolutionTest {

    private final static String RESOURCES_SMALL_FILE_TXT = "/small_file.txt";
    private final static int AHEAD = 11;

    // Iterate on this file
    private URI _file;
    private ElegantSolution _solution;

    // PREPARE

    @BeforeClass
    public static void classSetUp() throws Exception {
        System.out.println("Working Directory is " + System.getProperty("user.dir"));
    }

    @Before
    public void setUp() throws Exception {
        _file = NotOptimalSolutionTest.class.getResource(RESOURCES_SMALL_FILE_TXT).toURI();
    }

    // TESTS

    @Test(expected = IllegalArgumentException.class)
    public void shouldReturnErrorIfStepAheadIsNotPositive() throws Exception {
        _solution = new ElegantSolution(-1);
    }

    @Test
    public void shouldReturnMaximumIfStepAheadIs11() throws Exception {
        _solution = new ElegantSolution(AHEAD);
        Assert.assertEquals(14, _solution.find(new File(_file)));
    }

    @Test
    public void shouldStepAheadIsZeroShouldReturnBiggestNumberMulByTwo() throws Exception {
        _solution = new ElegantSolution(0);
        Assert.assertEquals(26, _solution.find(new File(_file)));
    }

    @Test(expected = IllegalStateException.class)
    public void shouldRespondWithErrorIfThereIsNoSolution() throws Exception {
        _solution = new ElegantSolution(1);
        _solution.find(new File(_file));
    }

    @Test
    public void shouldBePossibleMaxToBeNegative() throws Exception {
        _solution = new ElegantSolution(4);

        // that was tricky - thanks TDD
        Assert.assertEquals(-1, _solution.find(new File(_file)));
    }



}
