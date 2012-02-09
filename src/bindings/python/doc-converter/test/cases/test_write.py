#!/usr/bin/env python

import unittest
import rewrite_pydoc
import tempfile

class TestWrite(unittest.TestCase):

    def test_file_write_action(self):
        tmpfile = tempfile.mkstemp()[1]
        rewrite_pydoc.write_output_file(tmpfile, 'The contents.\n')

    def test_file_write_contents(self):
        tmpfile = tempfile.mkstemp()[1]
        rewrite_pydoc.write_output_file(tmpfile, 'The contents.\n')
        istream = open(tmpfile, 'r')
        contents = istream.read()
        self.assertEqual(contents, 'The contents.\n')

def main():
    unittest.main()

if __name__ == '__main__':
    main()
