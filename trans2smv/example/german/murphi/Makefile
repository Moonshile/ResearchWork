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

main: german.m
	make murphi
	make german
murphi: german.m
	${SRCPATH}/mu german.m > makeLog.txt
german: german.cpp
	${CXX} ${CFLAGS} ${OFLAGS} -o  german  german.cpp -I ${INCLUDEPATH} -lm
