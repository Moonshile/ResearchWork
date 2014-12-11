INCLUDEPATH = ~/Tools/cmurphi5.4.9/include
SRCPATH = ~/Tools/cmurphi5.4.9/src/

CXX = g++

CFLAGS = 

# optimization
OFLAGS = -ggdb
#OFLAGS = -O2

#Murphi options
MURPHIOPTS = -b -c

# rules for compiling

main: godson-t-repAll-protect.m
	make murphi
	make godson-t
murphi: godson-t-repAll-protect.m
	${SRCPATH}/mu godson-t-repAll-protect.m > makeLog.txt
godson-t: godson-t-repAll-protect.cpp
	${CXX} ${CFLAGS} ${OFLAGS} -o  godson-t-repAll-protect  godson-t-repAll-protect.cpp -I ${INCLUDEPATH} -lm