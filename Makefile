.PHONY: all clean mrproper py3

TARGET=oberon7

PARSERSRC=oberon7.g
SRC=main.cpp compiler.cpp symbols.cpp oberon7Lexer.cpp oberon7Parser.cpp
OBJS=$(subst .cpp,.o,$(SRC))
LIBS=-l antlr4-runtime
INCLUDES=-I /usr/include/antlr4-runtime/
HFILES=oberon7.hpp symbols.h
# CPPFLAGS=-g `llvm-config --cxxflags --ldflags --libs`
CPPFLAGS=-g -I/usr/include \
 -D_GNU_SOURCE \
 -D__STDC_CONSTANT_MACROS \
 -D__STDC_FORMAT_MACROS -D__STDC_LIMIT_MACROS \
 `llvm-config --ldflags --libs`
 # -std=c++14  # ANTLR braks
 # -fno-exceptions #ANTLR breaks

GPP=g++

all: oberon7

oberon7Lexer.cpp oberon7Parser.cpp: $(PARSERSRC)
	antlr4 -Dlanguage=Cpp $<

py3: $(PARSERSRC)
	antlr4 -Dlanguage=Python3 $<

%.o: %.cpp
	$(GPP) -c $< -o $@ $(INCLUDES)

symbols.cpp: symbols.h

$(TARGET): $(SRC)
	@echo $(OBJS)
	$(GPP) -o $@ $(SRC) $(LIBS) $(INCLUDES) $(CPPFLAGS)

clean:
	rm -f *.o

mrproper: clean
	rm -f $(TARGET) $(PARSERSRCCPP) *.tokens *.interp

test: $(TARGET)
	./$(TARGET) test-data/fibonacci.oberon
