#!/usr/bin/env python

import unittest
import rewrite_pydoc
import tempfile
import os

# The approach here, using the for loop and the 'ch' function, came from a
# reply to a StackOverflow question by one Adrian Panasiuk on 2009-07-28:
# http://stackoverflow.com/questions/1193909/pythons-unittest-and-dynamic-creation-of-test-cases

class Tests(unittest.TestCase):

    def check(self, testinput, testoutput):
        args       = rewrite_pydoc.parse_cmdline(['-f', testinput, '-o', 'ignored'])
        fname      = rewrite_pydoc.get_input_file_name(args)
        contents   = rewrite_pydoc.read_file_contents(fname)
        rewritten  = rewrite_pydoc.rewrite(contents, '../../../../docs/src/common-text',
                                           '../../../../docs/src/graphics', False)
        expected   = open(testoutput, 'r').read()
        self.assertEqual(rewritten, expected)


for i in range(2, 1000):
    testinput  = 'test/data/%.3d.i' % i
    testoutput = 'test/data/%.3d_expected.i' % i
    if os.path.exists(testinput):
        def ch(testinput, testoutput):
            return lambda self: self.check(testinput, testoutput)
        setattr(Tests, "test_%.3d" % i, ch(testinput, testoutput))

def main():
    unittest.main()

if __name__ == '__main__':
    main()
