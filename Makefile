.PHONY: all clean mrproper py3

TARGET=oberon7

PARSERSRC=oberon7.g
SRC=main.cpp compiler.cpp symbols.cpp oberon7Lexer.cpp oberon7Parser.cpp
OBJS=$(subst .cpp,.o,$(SRC))
LIBS=-l antlr4-runtime
INCLUDES=-I /usr/include/antlr4-runtime/
HFILES=oberon7.hpp symbols.h
CPPFLAGS=-g

all: oberon7

oberon7Lexer.cpp oberon7Parser.cpp: $(PARSERSRC)
	antlr4 -Dlanguage=Cpp $<

py3: $(PARSERSRC)
	antlr4 -Dlanguage=Python3 $<

%.o: %.cpp
	g++ -c $< -o $@ $(INCLUDES)

symbols.cpp: symbols.h

$(TARGET): $(SRC)
	@echo $(OBJS)
	g++ -o $@ $(SRC) $(LIBS) $(INCLUDES) $(CPPFLAGS)

clean:
	rm -f *.o

mrproper: clean
	rm -f $(TARGET) $(PARSERSRCCPP) *.tokens *.interp

test: $(TARGET)
	./$(TARGET) test-data/fibonacci.oberon
