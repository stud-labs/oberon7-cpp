#include <vector>
#include <map>
#include <string>
#include <memory>

using namespace std;

namespace o7c {

  class Scope;

  Scope * currentScope = NULL;

  class Symbol {
  public:
    const string name;
    const Scope * scope;
    Symbol(const string m_name, const Scope * m_scope = currentScope)
      : name(m_name), scope(m_scope) {};
  };

  class Type: public Symbol {
  public:
    const Type * parent;
    Type(const string m_name, const Type * parentType = NULL,
         const Scope * m_scope = currentScope)
      : Symbol(m_name, m_scope), parent(parentType) {};
  };

  class Variable: public Symbol {
  public:
    const Type * type;
    Variable(const string m_name, const Type * m_type, const Scope * m_scope = currentScope)
      : Symbol(m_name, m_scope), type(m_type) {};
  };

  class Scope: public Symbol {
  public:
    map<string, Symbol *> symbolTable;
    Scope(const string m_name, const Scope * m_scope = currentScope) // The scope name defines module /
      // procedure / function name
      : Symbol(m_name, m_scope) { currentScope = this; }
    static void initDefaultTypes();
  };

}
