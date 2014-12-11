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

main: godson-t-ce-0.m godson-t-ce-1-1lock.m godson-t-ce-2-inProtection.m godson-t-ce-3-replaceAll.m godson-t-ce-4-repAll-1lock.m
	make murphi
	make godson-t
	make -i run
murphi: godson-t-ce-0.m godson-t-ce-1-1lock.m godson-t-ce-2-inProtection.m godson-t-ce-3-replaceAll.m godson-t-ce-4-repAll-1lock.m
	${SRCPATH}/mu godson-t-ce-0.m > makelog.txt
	${SRCPATH}/mu godson-t-ce-1-1lock.m > makelog.txt
	${SRCPATH}/mu godson-t-ce-2-inProtection.m > makelog.txt
	${SRCPATH}/mu godson-t-ce-3-replaceAll.m > makelog.txt
	${SRCPATH}/mu godson-t-ce-4-repAll-1lock.m > makelog.txt
godson-t: godson-t-ce-0.cpp godson-t-ce-1-1lock.cpp godson-t-ce-2-inProtection.cpp godson-t-ce-3-replaceAll.cpp godson-t-ce-4-repAll-1lock.cpp
	${CXX} ${CFLAGS} ${OFLAGS} -o  godson-t-ce-0  godson-t-ce-0.cpp -I ${INCLUDEPATH} -lm
	${CXX} ${CFLAGS} ${OFLAGS} -o  godson-t-ce-1-1lock  godson-t-ce-1-1lock.cpp -I ${INCLUDEPATH} -lm
	${CXX} ${CFLAGS} ${OFLAGS} -o  godson-t-ce-2-inProtection  godson-t-ce-2-inProtection.cpp -I ${INCLUDEPATH} -lm
	${CXX} ${CFLAGS} ${OFLAGS} -o  godson-t-ce-3-replaceAll  godson-t-ce-3-replaceAll.cpp -I ${INCLUDEPATH} -lm
	${CXX} ${CFLAGS} ${OFLAGS} -o  godson-t-ce-4-repAll-1lock  godson-t-ce-4-repAll-1lock.cpp -I ${INCLUDEPATH} -lm
run: godson-t-ce-0 godson-t-ce-1-1lock godson-t-ce-2-inProtection godson-t-ce-3-replaceAll
	./godson-t-ce-0 -m1024 -td > ce0.txt
	./godson-t-ce-1-1lock -m1024 -td > ce1.txt
	./godson-t-ce-2-inProtection -m1024 -td > ce2.txt
	./godson-t-ce-3-replaceAll -m1024 -td > ce3.txt
	./godson-t-ce-4-repAll-1lock -m1024 -td > ce4.txt
	echo -e "-----\nDone!\nSee ce*.txt for counterexample results.\n-----"
