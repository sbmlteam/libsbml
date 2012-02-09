#!/usr/bin/env python

import unittest
import rewrite_pydoc

class TestRead(unittest.TestCase):

    def test_file_read(self):
        args  = rewrite_pydoc.parse_cmdline(['-f', 'test/data/001.i', '-o', 'ignored.bar'])
        fname = rewrite_pydoc.get_input_file_name(args)
        contents = rewrite_pydoc.read_file_contents(fname)
        self.assertEqual(contents, 'The contents.\n')



def main():
    unittest.main()

if __name__ == '__main__':
    main()
