.PHONY: all clean mrproper

TARGET=oberon7

PARSERSRC=oberon7.g
SRC=main.cpp compiler.cpp oberon7Lexer.cpp oberon7Parser.cpp
OBJS=$(subst .cpp,.o,$(SRC))
LIBS=-l antlr4-runtime
INCLUDES=-I /usr/include/antlr4-runtime/
HFILES=oberon7.hpp

all: oberon7

oberon7Lexer.cpp oberon7Parser.cpp: $(PARSERSRC)
	antlr4 -Dlanguage=Cpp $<

%.o: %.cpp
	g++ -c $< -o $@ $(INCLUDES)

$(TARGET): $(SRC)
	@echo $(OBJS)
	g++ -o $@ $(SRC) $(LIBS) $(INCLUDES)

clean:
	rm -f *.o

mrproper: clean
	rm -f $(TARGET) $(PARSERSRCCPP) *.tokens *.interp
