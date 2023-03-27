.PHONY: all clean mrproper test ret py3

TARGET=oberon7

PARSERSRC=oberon7.g
SRC=main.cpp symbols.cpp compiler.cpp oberon7Lexer.cpp oberon7Parser.cpp
OBJS=$(subst .cpp,.o,$(SRC))
DEPENDS := $(patsubst %.cpp,%.d,$(SRC))
LIBS=-l antlr4-runtime
INCLUDES=-I /usr/include/antlr4-runtime/
HFILES=oberon7.hpp

 # -O2
CPPFLAGS=-g \
    -I/usr/include \
 -D_GNU_SOURCE \
 -D__STDC_CONSTANT_MACROS \
    -D__STDC_FORMAT_MACROS -D__STDC_LIMIT_MACROS \
 `llvm-config --ldflags --libs`
 # -std=c++14  # ANTLR braks
 # -fno-exceptions #ANTLR breaks

GPP=g++
INCLUDES=-I /usr/include/antlr4-runtime/
HFILES=oberon7.hpp
TD=test-data

-include $(DEPENDS)

all: oberon7

oberon7Lexer.cpp oberon7Parser.cpp: $(PARSERSRC)
	antlr4 -Dlanguage=Cpp $<

py3: $(PARSERSRC)
	antlr4 -Dlanguage=Python3 $<

%.o : %.cpp Makefile
	$(GPP) -MMD -MP -c $< -o $@ $(INCLUDES) $(CPPFLAGS)

$(TARGET): $(OBJS)
	# @echo $(OBJS)
	$(GPP) -o $@ $^ $(LIBS) $(INCLUDES) $(CPPFLAGS)

clean:
	rm -f *.o

mrproper: clean
	rm -f $(TARGET) $(PARSERSRCCPP) *.tokens *.interp
	rm -f *.orig $(TD)/*.orig

test: $(TARGET)
	./$(TARGET) $(TD)/fibonacci.oberon

ret: $(TARGET)
	./$(TARGET) $(TD)/ret.oberon
