print "2. Python started"

from com.smook import SampleTest

import pytest

def main():
    fc = SampleTest(3, 10)

    print dir(pytest)
    pytest.set_trace() ## DEBUG ##

    print [fc.calc() for i in dir(pytest)]

    print "Go go smook..."
