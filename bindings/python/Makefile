CC  = gcc
CXX = g++

PYTHON   = /usr/local/bin/python2.3
CPPFLAGS = -I/usr/local/include/python2.3
CFLAGS   = -bundle -bundle_loader $(PYTHON)
LDFLAGS  = -L/usr/local/lib/python2.3/config/
LIBS     = -lsbml -lxerces-c


all: _libsbml.so

_libsbml.so: libsbml_wrap.cpp
	$(CXX) $(CPPFLAGS) $(CFLAGS) $(LDFLAGS) $< -o $@ $(LIBS)

libsbml_wrap.cpp: ../swig/libsbml.i
	swig -c++ -python -o libsbml_wrap.cpp ../swig/libsbml.i

clean:
	rm -f *~ *.o *.so *.pyc libsbml.py libsbml_wrap.cpp
