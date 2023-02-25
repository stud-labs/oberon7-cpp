.PHONY: all clean mrproper

TARGET=oberon7

PARSERSRC=oberon7.g
PARSERSRCCPP=parser.cpp
SRC=main.cpp copiler.cpp $(PARSERSRCCPP)
OBJS=$(subst .cpp,.o,$(SRC))
LIBS=
INCLUDES=
HFILES=oberon7.hpp

all: oberon7

%.o: %.cpp
	$(CC) -c $< -o $@ $(INCLUDES)

$(TARGET): $(OBJ)
	g++ -o $@ $(OBJS) $(LIBS)


clean:
	rm -f *.o

mrproper: clean
	rm -f $(TARGET) $(PARSERSRCCPP)
