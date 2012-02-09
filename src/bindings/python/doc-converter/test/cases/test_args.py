#!/usr/bin/env python

import unittest
import rewrite_pydoc

class TestArgs(unittest.TestCase):

    def test_input_file_arg(self):
        args  = rewrite_pydoc.parse_cmdline(['-f', 'foo.bar', '-o', 'ignored.bar'])
        fname = rewrite_pydoc.get_input_file_name(args)
        self.assertEqual(fname, 'foo.bar')

    def test_output_file_arg(self):
        args  = rewrite_pydoc.parse_cmdline(['-f', 'ignored.bar', '-o', 'output.zap'])
        fname = rewrite_pydoc.get_output_file_name(args)
        self.assertEqual(fname, 'output.zap')

    def test_inclue_arg(self):
        args    = rewrite_pydoc.parse_cmdline(['-f', 'ignored.bar', '-o', 'output.zap', '-i', 'foo'])
        dirname = rewrite_pydoc.get_include_dir(args)
        self.assertEqual(dirname, 'foo')

    def test_graphics_arg(self):
        args    = rewrite_pydoc.parse_cmdline(['-f', 'ignored.bar', '-o', 'output.zap', '-g', 'foo'])
        dirname = rewrite_pydoc.get_graphics_dir(args)
        self.assertEqual(dirname, 'foo')


def main():
    unittest.main()

if __name__ == '__main__':
    main()
