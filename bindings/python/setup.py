import distutils
from distutils.core import setup, Extension

setup(name = "libSBML Python", version = "1.0",
      ext_modules = [Extension("_libsbml", ["libsbml.i", "libsbml.cpp"])])
