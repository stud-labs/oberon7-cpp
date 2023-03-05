#include <iostream>
#include "oberon7Lexer.h"
#include "oberon7Parser.h"
#include <antlr4-runtime.h>
#include "compiler.h"

using namespace std;
using namespace antlr4;


int main(int argc, const char* argv[]) {
  cout << "Oberon7 (hopefully) LLVM compiler \n" << endl;
  ifstream stream;
  stream.open(argv[1]);
  ANTLRInputStream input(stream);
  oberon7Lexer lexer(&input);
  CommonTokenStream tokens(&lexer);
  oberon7Parser parser(&tokens);

  // tree::ParseTree *tree = parser.key();
  // TreeShapeListener listener;
  // tree::ParseTreeWalker::DEFAULT.walk(&listener, tree);

  parser.module(); // TODO: Use module returned context

  return 0;
}
