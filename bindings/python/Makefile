CC  = gcc
CXX = g++

#linux settings?
#PYTHON   = /usr/local/bin/python2.3
#CPPFLAGS = -I/usr/local/include/python2.3
#CFLAGS   = -bundle -bundle_loader $(PYTHON)
#LDFLAGS  = -L/usr/local/lib/python2.3/config/
#LIBS     = -lsbml -lxerces-c

#G5 settings
PYTHON   = /usr/bin/python
CPPFLAGS = -I/System/Library/Frameworks/Python.framework/Versions/2.3/include/python2.3 -I../../src
CFLAGS   = -bundle -framework Python
LDFLAGS  = -L/System/Library/Frameworks/Python.framework/Versions/2.3/lib/python2.3/lib-dynload -L$(HOME)/software/lib
LIBS     = -lsbml -lxerces-c


all: _libsbml.so

_libsbml.so: libsbml_wrap.cpp
	$(CXX) $(CPPFLAGS) $(CFLAGS) $(LDFLAGS) $< -o $@ $(LIBS)

libsbml_wrap.cpp: ../swig/libsbml.i
	swig -c++ -python -o libsbml_wrap.cpp ../swig/libsbml.i

clean:
	rm -f *~ *.o *.so *.pyc libsbml.py libsbml_wrap.cpp
