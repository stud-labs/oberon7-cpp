.PHONY: all clean mrproper test ret

TARGET=oberon7

PARSERSRC=oberon7.g
SRC=main.cpp compiler.cpp oberon7Lexer.cpp oberon7Parser.cpp
OBJS=$(subst .cpp,.o,$(SRC))
LIBS=-l antlr4-runtime \
    -I/usr/include \
    -D_GNU_SOURCE -D__STDC_CONSTANT_MACROS \
    -D__STDC_FORMAT_MACROS -D__STDC_LIMIT_MACROS \
    `llvm-config --libs`

INCLUDES=-I /usr/include/antlr4-runtime/
HFILES=oberon7.hpp
TD=test-data

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

test: $(TARGET)
	./$(TARGET) $(TD)/fibonacci.oberon

ret: $(TARGET)
	./$(TARGET) $(TD)/ret.oberon
