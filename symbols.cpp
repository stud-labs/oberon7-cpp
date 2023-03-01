#include "symbols.h"

using namespace std;

namespace o7c {

  Scope * currentScope = NULL;

  bool textEqual(char * a, char *b) {
    string sa(a);
    string sb(b);
    return sa==sb;
  }

}
