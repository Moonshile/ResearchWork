
INCLUDE_PATH = ./

CC = g++

OPTIONS = -Wall --std=c++11 -g

checker: objects
	${CC} -o checker ${OPTIONS} task.o uniserv.o checker.o -I ${INCLUDE_PATH}

objects: task.cpp uniserv.cpp
	${CC} -c ${OPTIONS} task.cpp -I ${INCLUDE_PATH}
	${CC} -c ${OPTIONS} uniserv.cpp -I ${INCLUDE_PATH}
	${CC} -c ${OPTIONS} checker.cpp -I ${INCLUDE_PATH}

clean:
	rm *o
	rm checker
