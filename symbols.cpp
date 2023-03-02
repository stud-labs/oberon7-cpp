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

  ostream& operator<<(ostream& os, const Symbol& s) {
    s.printOn(os);
    return os;
  }

  ostream& operator<<(ostream& os, const Symbol * s) {
    s->printOn(os);
    return os;
  }

  void Symbol::printOn(ostream& os) const {
    os << className() << " '" << name << "'";
  }

  void Type::printOn(ostream& os) const {
    Symbol::printOn(os);
    if (parent) {
      os << " subtype of " << parent;
    }
  }

  void Variable::printOn(ostream& os) const {
    Symbol::printOn(os);
    if (type) {
      os << ": " << type;
    } else {
      os << " (no type)";
    }
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

  void Scope::printSymbolTable(ostream& os) const {
    os << "Scope:" << name << endl;
    for (const auto &kv: symbolTable) {
      string type = "NULL";
      Variable * v = (Variable *) kv.second;
      if (v->type) {
        type = v->type->name;
      }
      cout << kv.first << "->" << v << endl;
    }
  }

  void Scope::printOn(ostream& os) const {
    printSymbolTable(os);
  }
}
