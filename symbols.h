#include <vector>
#include <map>
#include <string>
#include <memory>
#include <iostream>

#ifndef __SYMBOLS_H__
#define __SYMBOLS_H__

using namespace std;

namespace o7c {

  class Scope;

  extern Scope * currentScope;

  class Symbol {
  public:
    const string name;
    const Scope * scope;
    Symbol(const string m_name, const Scope * m_scope = currentScope)
      : name(m_name), scope(m_scope) {};
    friend ostream& operator<<(ostream& os, const Symbol& sym);
    friend ostream& operator<<(ostream& os, const Symbol * sym);

  protected:
    virtual void printOn(ostream&) const;
    virtual const string className() const {return "Symbol";};
  };

  class Type: public Symbol {
  public:
    const Type * parent;
    Type(const string m_name, const Type * parentType = NULL,
         const Scope * m_scope = currentScope)
      : Symbol(m_name, m_scope), parent(parentType) {};
    void printOn(ostream&) const override;
  };

  class Variable: public Symbol {
  public:
    const Type * type;
    Variable(const string m_name, const Type * m_type, const Scope * m_scope = currentScope)
      : Symbol(m_name, m_scope), type(m_type) {};

    void printOn(ostream&) const override;

  private:
    const string className() const override {return "Variable";};
  };

  class Scope: public Symbol {
  public:
    map<string, Symbol *> symbolTable;
    Scope(const string m_name, const Scope * m_scope = currentScope) // The scope name defines module /
      // procedure / function name
      : Symbol(m_name, m_scope) { currentScope = this; }
    static void initDefaultTypes();
    void addVariables(vector<string> &v, Type * t);
    void addVar(string &v, Type * t);
    void printSymbolTable(ostream& os = cout) const;
  private:
    void printOn(ostream& os) const override;
  };

  bool textEqual(char * a, char *b);
}


#endif // __SYMBOLS_H__
