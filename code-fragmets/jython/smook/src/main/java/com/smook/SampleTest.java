package com.smook;

/**
 * Fibonacci sequence calculator.
 */
public class SampleTest {

    private int a;
    private int b;

    public SampleTest() {
        this(0, 1);
    }

    /**
     * Create a fibonacci sequence starting at start.
     *
     * @param starta
     * @param startb
     */
    public SampleTest(int starta, int startb) {
        this.a = starta;
        this.b = startb;
    }

    /**
     * @return next value
     */
    public int calc() {

        if (a == 10 && b == 11) {
            System.out.println("Dude Jython is awesome");
        }

        int next = this.a + this.b;
        if (this.a > this.b) {
            this.b = next;

        } else {
            this.a = next;
        }
        return next;
    }

    public boolean hasNext() {
        return true;
    }

    public Integer next() {
        return calc();
    }

    public void remove() {
        throw new IllegalStateException("not a valid call");
    }
}
