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
  class Params;

  extern Scope * currentScope;

  class Symbol {
  public:
    const string name;
    Scope * scope;
    Symbol(const string m_name, Scope * m_scope = currentScope)
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
         Scope * m_scope = currentScope)
      : Symbol(m_name, m_scope), parent(parentType) {};
    void printOn(ostream&) const override;
  };

  class Variable: public Symbol {
  public:
    const Type * type;
    bool var;
    Variable(const string m_name, const Type * m_type, Scope * m_scope = currentScope)
      : Symbol(m_name, m_scope), type(m_type), var(false) {};

    void setVar(bool m_var) {var = m_var; };

  protected:
    void printOn(ostream&) const override;
    const string className() const override {return "Variable";};
  };

  class Func: public Variable {
  public:
    Params * params;
    Func(const string m_name, Params * m_params, const Type * m_type=NULL,
         Scope * m_scope=currentScope)
      : Variable(m_name, m_type, m_scope), params(m_params) {}
  protected:
    virtual const string className() const {return "Func";};
    void printOn(ostream& os) const override;
  };

  class Scope: public Symbol {
  public:
    map<string, Symbol *> symbolTable;
    Scope(const string m_name, Scope * m_scope = currentScope) // The scope name defines module /
      // procedure / function name
      : Symbol(m_name, m_scope) { currentScope = this; }
    static void initDefaultTypes();
    bool addVariables(vector<string> &v, Type * t, string var = "");
    virtual bool addVar(string &v, Type * t, string var = "");
    void addFunc(const string name, Func * func);
    void printSymbolTable(ostream& os = cout) const;
  protected:
    void printOn(ostream& os) const override;
  };

  class Params: public Scope {
  public:
    vector<string> params;
  public:
    Params(const string m_name, Scope * m_scope = currentScope)
      : Scope(m_name, m_scope) {};
    bool addVar(string &v, Type * t, string var = "") override;
  protected:
    virtual const string className() const {return "Params";};
  };

  bool textEqual(char * a, char *b);
}


#endif // __SYMBOLS_H__
