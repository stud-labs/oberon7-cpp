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
  class Func;

  extern Scope * currentScope;

  class Symbol {
  public:
    Scope * scope;
    Symbol(Scope * m_scope = currentScope)
      : scope(m_scope) {};
    friend ostream& operator<<(ostream& os, const Symbol& sym);
    friend ostream& operator<<(ostream& os, const Symbol * sym);

  protected:
    virtual void printOn(ostream&) const;
    virtual const string className() const {return "Symbol";};
    virtual ~Symbol() {};
  };

  class NamedSymbol: public Symbol {
  public:
    const string name;
    NamedSymbol(const string m_name, Scope * m_scope = currentScope)
      : Symbol(m_scope), name(m_name) {};

  protected:
    void printOn(ostream&) const override;
    const string className() const override {return "NamedSymbol";};
  };

  class Qual: public Symbol {
  public:
    vector<string> qual;
    Qual(vector<string> m_qual, Scope * m_scope=currentScope)
      : Symbol(m_scope), qual(m_qual) {};

  protected:
    const string className() const override {return "Qual";};
    void printOn(ostream& os) const override;
  };

  class Type: public NamedSymbol {
  public:
    Type(const string m_name, Scope * m_scope = currentScope)
      : NamedSymbol(m_name, m_scope) {};
  protected:
    void printOn(ostream&) const override;
    const string className() const override {return "Type";};
    ~Type() {};
  };

  class SubType: public Type {
  public:
    const Type * parent;
    SubType(const string m_name, const Type * parentType = NULL,
         Scope * m_scope = currentScope)
      : Type(m_name, m_scope), parent(parentType) {};
  protected:
    void printOn(ostream&) const override;
    const string className() const override {return "SubType";};
    ~SubType() {};
  };

  class QualType: public Type {
  public:
    const Qual * qual;
    QualType(const Qual * m_qual, Scope * m_scope = currentScope)
      : Type("aQual", m_scope), qual(m_qual) {};
  protected:
    virtual const string className() const override {return "QualType";};
    void printOn(ostream&) const override;
    virtual ~QualType() {delete qual;};
  };

  class Variable: public NamedSymbol {
  public:
    const Type * type;
    Variable(const string m_name, const Type * m_type, Scope * m_scope = currentScope)
      : NamedSymbol(m_name, m_scope), type(m_type) {};
  protected:
    void printOn(ostream&) const override;
    const string className() const override {return "Variable";};
  };

  class VarVariable: public Variable {
  public:
    VarVariable(const string m_name, const Type * m_type, Scope * m_scope = currentScope)
      : Variable(m_name, m_type, m_scope) {};
  protected:
    const string className() const override {return "VarVariable";};
  };

  class Scope: public NamedSymbol {
  public:
    map<string, Symbol *> symbolTable;
    Scope(const string m_name, Scope * m_scope = currentScope) // The scope name defines module /
      // procedure / function name
      : NamedSymbol(m_name, m_scope) { currentScope = this; }
    static void initDefaultTypes();
    bool addVariables(vector<string> &v, Type * t, string var = "");
    virtual bool addVar(string &v, Type * t, string var = "");
    void addFunc(const string name, Func * func);
    void printSymbolTable(ostream& os = cout) const;
  protected:
    const string className() const override {return "Func";};
    void printOn(ostream& os) const override;
    virtual ~Scope() {}; // TODO Release Scope elements
  };

  class Params: public Scope {
  public:
    vector<string> params;
  public:
    Params(const string m_name, Scope * m_scope = currentScope)
      : Scope(m_name, m_scope) {};
    bool addVar(string &v, Type * t, string var = "") override;
  protected:
    const string className() const override {return "Params";};
  };

  class Func: public Variable {
  public:
    Params * params;
    Func(const string m_name, Params * m_params, const Type * m_type=NULL,
         Scope * m_scope=currentScope)
      : Variable(m_name, m_type, m_scope), params(m_params) {}
  protected:
    const string className() const override {return "Func";};
    void printOn(ostream& os) const override;
    ~Func() {delete params;};
  };

  class Array: public SubType {
  public:
    vector<int> dims;
    Array(vector<int> m_dims, Type * m_arrayType, Scope * m_scope=currentScope)
      : SubType("anArray", m_arrayType, m_scope), dims(m_dims) {};
  protected:
    const string className() const override {return "Array";};
    void printOn(ostream& os) const override;
  };

  class Record: public Type {
  public:
    Record(const string m_name, Scope * m_scope = currentScope)
      : Type(m_name, m_scope) {}; // TODO: Arguments
  };

  class Pointer: public SubType {
  public:
    Pointer(const string m_name, const Type * m_type, Scope * m_scope = currentScope)
      : SubType(m_name, m_type, m_scope) {};
  protected:
    const string className() const override {return "Pointer";};
  };

  class ProcType: public SubType {
  public:
    Params * params;
    ProcType(const string m_name, Params * m_params, const Type * m_type=NULL,
             Scope * m_scope=currentScope)
      : SubType(m_name, m_type, m_scope), params(m_params) {}; // TODO: arguments
  protected:
    const string className() const override {return "FuncType";};
    ~ProcType() {delete params;};
  };

  bool textEqual(char * a, char *b);
}


#endif // __SYMBOLS_H__
