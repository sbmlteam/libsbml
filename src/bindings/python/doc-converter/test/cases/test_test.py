#!/usr/bin/env python

import unittest

class TestTest(unittest.TestCase):

    def test_true(self):
        self.failUnless(True)

def main():
    unittest.main()

if __name__ == '__main__':
    main()
