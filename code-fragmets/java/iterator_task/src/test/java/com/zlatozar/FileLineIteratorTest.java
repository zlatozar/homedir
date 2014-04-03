package com.zlatozar;

import org.junit.Assert;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import java.io.File;
import java.net.URI;
import java.util.List;

/**
 * Tests for {@link com.zlatozar.FileLineIterator}
 *
 * @author zlatozar@gmail.com
 */
public class FileLineIteratorTest {

    private final static String RESOURCES_SMALL_FILE_TXT = "/small_file.txt";
    private final static String RESOURCES_EMPTY_FILE_TXT = "/empty_file.txt";
    private final static int LINE_NUMBERS = 13;

    // Iterate on this file
    private URI _file;
    private FileLineIterator _iter;

    // PREPARE

    @BeforeClass
    public static void classSetUp() throws Exception {
        System.out.println("Working Directory is " + System.getProperty("user.dir"));
    }

    @Before
    public void setUp() throws Exception {
        _file = FileLineIteratorTest.class.getResource(RESOURCES_SMALL_FILE_TXT).toURI();
        _iter = new FileLineIterator(new File(_file));
    }

    // TESTS

    @Test
    public void shouldFindAndLoadFile() throws Exception {
        Assert.assertNotNull(new FileLineIterator(new File(_file)));
    }

    @Test
    public void shouldBePossibleToFindFirstValid() throws Exception {
        _iter.next();
        Assert.assertTrue(_iter.next().get(1) == 1);
    }

    @Test
    public void shouldCountAllLineNumbers() throws Exception {
        int i = 0;
        for (; i < LINE_NUMBERS; i++) {
            _iter.next();
        }

        Assert.assertEquals(LINE_NUMBERS, i);
    }

    @Test
    public void shouldBePossibleToUseHasNext() throws Exception {
        int i = 0;
        while (_iter.hasNext()) {
            _iter.next();
            i++;
        }

        Assert.assertEquals(LINE_NUMBERS, i);
    }

    @Test
    public void shouldBePossibleToFindOutCurrentLineNumber() throws Exception {
        _iter.next();
        _iter.next();

        Assert.assertEquals(2, _iter.getCurrentLineNumber());
    }

    @Test
    public void shouldBePossibleToFindOutNotValidLines() throws Exception {
        final List<Integer> notValid = _iter.next();

        Assert.assertEquals(1, notValid.size());
    }

    @Test
    public void shouldBePossibleToFindOutValidLines() throws Exception {
        _iter.next();
        final List<Integer> notValid = _iter.next();

        Assert.assertEquals(2, notValid.size());
    }

    @Test
    public void shouldNotCrashIfFileIsEmpty() throws Exception {
        final URI file = FileLineIteratorTest.class.getResource(RESOURCES_EMPTY_FILE_TXT).toURI();
        final FileLineIterator iter = new FileLineIterator(new File(file));

        Assert.assertFalse(iter.hasNext());
    }
}
