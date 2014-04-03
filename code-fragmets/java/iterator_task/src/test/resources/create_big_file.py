#!/usr/local/bin/python

# If I have time will finish it

def __get_random_int(length):
    rsize = '{0:0%s}' % length
    return rsize.format(random.randint(1, 10000000))[:length - 2]

def __get_random_alphanumeric(length):
    return str(uuid4())[:length - 2]
