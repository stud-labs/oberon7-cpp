import sys
from antlr4 import *
from oberon7Lexer import oberon7Lexer
from oberon7Parser import oberon7Parser
 
def main(argv):
    input_stream = FileStream(argv[1])
    lexer = oberon7Lexer(input_stream)
    stream = CommonTokenStream(lexer)
    parser = oberon7Parser(stream)
    tree = parser.module()
 
if __name__ == '__main__':
    main(sys.argv)