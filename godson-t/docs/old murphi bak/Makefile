INCLUDEPATH = ~/Tools/cmurphi5.4.9/include
SRCPATH = ~/Tools/cmurphi5.4.9/src/

CXX = g++

CFLAGS = 

# optimization
OFLAGS = -ggdb
#OFLAGS = -O2

#Murphi options
MURPHIOPTS = -b -c

#all: down lin pingpong sets sort5 down.cache lin.cache pingpong.cache sets.cache sort5.cache down.disk lin.disk pingpong.disk sets.disk sort5.disk 
#splitfile: lin.cache.splitfile pingpong.cache.splitfile sets.cache.splitfile sort5.cache.splitfile lin.disk.splitfile pingpong.disk.splitfile sets.disk.splitfile sort5.disk.splitfile

# rules for compiling

main: godson-t-ce.m godson-t-1lock.m godson-t-weakerP.m
	make murphi
	make godson-t
murphi: godson-t-ce.m godson-t-1lock.m godson-t-weakerP.m
	${SRCPATH}/mu godson-t-ce.m
	${SRCPATH}/mu godson-t-1lock.m
	${SRCPATH}/mu godson-t-weakerP.m
godson-t: godson-t-ce.cpp godson-t-1lock.cpp godson-t-weakerP.cpp
	${CXX} ${CFLAGS} ${OFLAGS} -o  godson-t-ce  godson-t-ce.cpp -I ${INCLUDEPATH} -lm
	${CXX} ${CFLAGS} ${OFLAGS} -o  godson-t-1lock  godson-t-1lock.cpp -I ${INCLUDEPATH} -lm
	${CXX} ${CFLAGS} ${OFLAGS} -o  godson-t-weakerP  godson-t-weakerP.cpp -I ${INCLUDEPATH} -lm

old: godson-t-ori-nolasting.m godson-t-ori-1lockinuse.m godson-t-origin.m
	make old-murphi
	make old-cpp
old-murphi: godson-t-ori-nolasting.m godson-t-ori-1lockinuse.m godson-t-origin.m
	${SRCPATH}/mu godson-t-ori-nolasting.m
	${SRCPATH}/mu godson-t-ori-1lockinuse.m
	${SRCPATH}/mu godson-t-origin.m
old-cpp: godson-t-ori-nolasting.cpp godson-t-ori-1lockinuse.cpp godson-t-origin.cpp
	${CXX} ${CFLAGS} ${OFLAGS} -o  godson-t-ori-nolasting  godson-t-ori-nolasting.cpp -I ${INCLUDEPATH} -lm
	${CXX} ${CFLAGS} ${OFLAGS} -o  godson-t-ori-1lockinuse  godson-t-ori-1lockinuse.cpp -I ${INCLUDEPATH} -lm
	${CXX} ${CFLAGS} ${OFLAGS} -o  godson-t-origin  godson-t-origin.cpp -I ${INCLUDEPATH} -lm