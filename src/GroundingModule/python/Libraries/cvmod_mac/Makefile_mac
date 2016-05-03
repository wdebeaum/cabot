PYTHON_VERSION=2.7

all:
	swig -c++ -python cvmod.i
	g++ -O2 -fPIC -c cvmod_mac.cpp cvmod_wrap.cxx -I/usr/include/python$(PYTHON_VERSION) -I/opt/local/Library/Frameworks/Python.framework/Versions/$(PYTHON_VERSION)/lib/python$(PYTHON_VERSION)/site-packages/numpy/core/include -I/opt/local/include
	ld -bundle -flat_namespace -undefined suppress cvmod_mac.o cvmod_wrap.o -o _cvmod.so -framework OpenGL -framework GLUT
	cp _cvmod.so cvmod.py ../
clean:
	rm -rf build
	rm -f _cvmod.so cvmod.py cvmod_wrap.c cvmod_wrap.cxx *.o *~
