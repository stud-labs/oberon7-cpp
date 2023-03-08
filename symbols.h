#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include <memory>
#include <string>
#include "compiler.h"
#include <vector>
#include <map>
#include <string>
#include <memory>
#include <iostream>
#include "compiler.h"

#ifndef __SYMBOLS_H__
#define __SYMBOLS_H__

using namespace std;

namespace o7c {

  class Scope;
  class Params;
  class Func;
  class Qual;

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
  public:
    virtual bool isType() {return false;};
    virtual bool isVariable() {return false;};

    virtual llvm::Value * llvmValue() const {return (llvm::Value *) nullptr;};
    virtual llvm::Type * llvmType() const {return (llvm::Type *) nullptr;};
    virtual llvm::Constant * llvmConst(const string text) const {return (llvm::Constant *) nullptr;};
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
    bool isType() override {return true;};
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

    llvm::Type * llvmType() const override {return parent->llvmType();};
  protected:
    void printOn(ostream&) const override;
    const string className() const override {return "SubType";};
    ~SubType() {};
  };

  class QualType: public Type {
  public:
    Qual * qual;
    QualType(Qual * m_qual, Scope * m_scope = currentScope)
      : Type("aQual", m_scope), qual(m_qual) {};

    llvm::Type * llvmType() const override ;

  protected:
    virtual const string className() const override {return "QualType";};
    void printOn(ostream&) const override;
  public:
    virtual ~QualType() {delete qual;};
  };

  class Variable: public NamedSymbol {
  public:
    const Type * type;
    Variable(const string m_name, const Type * m_type, Scope * m_scope = currentScope)
      : NamedSymbol(m_name, m_scope), type(m_type) {};
    llvm::Type * llvmType() const override {return type->llvmType();};
    bool isVariable() override {return true;};
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
    llvm::Function * llvmFunc = NULL;
    Scope(const string m_name, Scope * m_scope = currentScope) // The scope name defines module /
      // procedure / function name
      : NamedSymbol(m_name, m_scope) { currentScope = this; }
    static void initDefaultTypes();

    bool addVariables(vector<string> &v, Type * t, string var = "");
    virtual bool addVar(string &v, Type * t, string var = "");
    void addFunc(const string name, Func * func);
    bool addType(Type * type);

    void printSymbolTable(ostream& os = cout) const;
    void setLLVMFunc(llvm::Function * m_func) {llvmFunc=m_func;};
    Symbol * findSymbol(const string id);
    Symbol * findSymbol(Qual * qual);
    Type * findType(Qual * qual);
    Variable * findVariable(Qual * qual);

    virtual ~Scope() {}; // TODO Release Scope elements
  protected:
    const string className() const override {return "Func";};
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


  class IntegerType: public Type {
  public:
    ssize_t numBits;
    IntegerType(const string m_name, const ssize_t m_numBits, Scope * m_scope = currentScope)
      : Type(m_name, m_scope), numBits(m_numBits) {}
    llvm::Type * llvmType() const override {
      return llvm::Type::getIntNTy(*Context, numBits);
    };
    llvm::Constant * llvmConst(const string text) const override {
      ssize_t radix = 10; // TODO: Figure out radix from text
      return llvm::ConstantInt::get((llvm::IntegerType *) llvmType(), text, radix);
    };

  };


  bool textEqual(char * a, char *b);

  void InitializeDefaultSymbols(Scope * scope);
}


#endif // __SYMBOLS_H__
