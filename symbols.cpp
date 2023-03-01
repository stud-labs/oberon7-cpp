#include "symbols.h"
#include <iostream>

using namespace std;

namespace o7c {

  Scope * currentScope = NULL;

  bool textEqual(char * a, char *b) {
    string sa(a);
    string sb(b);
    return sa==sb;
  }

  void Scope::addVariables(vector<string> &vars, Type * t) {
    for(auto v: vars) {
      addVar(v, t);
    }
  }

  void Scope::addVar(string &v, Type *t) {
    Variable * var = new Variable(v, t);
    symbolTable[v] = var;
  }

  void Scope::printSymbolTable() {
    cout << "Scope:" << name << endl;
    for (const auto &kv: symbolTable) {
      string type = "NULL";
      Variable * v = (Variable *) kv.second;
      if (v->type) {
        type = v->type->name;
      }
      cout << kv.first << "->" << v->name << ':' << type << endl;
    }
  }
}
