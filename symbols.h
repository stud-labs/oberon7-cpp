#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"

#include <iostream>
#include <string>
#include <vector>
#include <map>

#include "compiler.h"

#ifndef __SYMBOLS_H__
#define __SYMBOLS_H__

using namespace std;

namespace o7c {

  class Scope;
  class OberonModule;

  extern Scope * currentScope;

  class Symbol {

  public:
    Symbol() {};

    friend std::ostream&
      operator<<(std::ostream& os, const Symbol& s) {s.printOn(os); return os;}
    friend std::ostream&
      operator<<(std::ostream& os, const Symbol* s) {s->printOn(os); return os;}
  public:
    virtual void printOn(std::ostream& os) const {};
  public:
    virtual bool hasName() {return false;};
  };

  class NamedSymbol: public Symbol {
  public:
    const std::string name;
  public:
    NamedSymbol(const std::string m_name)
      : Symbol(), name(m_name) {};
  protected:
    bool hasName() override {return true;};
  public:
    void printOn(std::ostream& os) const override;
  };

  class Type: public NamedSymbol {
  public:
    Type(const std::string m_name)
      : NamedSymbol(m_name) {};
  public:
    void printOn(std::ostream& os) const override;
    virtual llvm::Value * convertFrom(llvm::Value * v) {return v;};
    virtual llvm::Type * llvmType() {return llvm::Type::getVoidTy(*Context); }; // TODO Not Implemented
  };


  // class DerivedType: Type {

  // };

  class Variable: public NamedSymbol {
  public:
    Type * type;
    Variable(const std::string m_name, Type * m_type)
      : NamedSymbol(m_name), type(m_type) {};
  public:
    void printOn(std::ostream& os) const override;
    Type * setType(Type * ty);
  };

  class Func: public Variable {
  public:
    const Scope * vars;
    Func(const std::string m_name,
         Type * m_type,
         const Scope * m_vars)
      : Variable(m_name, m_type), vars(m_vars) {};
  };

  class Scope: public Symbol {
    std::vector<Symbol *> symbols;
    std::map<std::string, Symbol *> symbolTable;
  public:
    Scope * parent;
  public:
    Scope(const std::vector<Symbol *> m_symbols)
      : Symbol(),
        symbols(m_symbols),
        parent(currentScope) {makeSymbolTable();};
    Scope()
      : Symbol(), parent(currentScope) {}
    Symbol * add(Symbol * s);
    // Scope * setParent(Scope * m_parent) {
    //   parent = m_parent;
    //   return m_parent;
    // }
    Symbol * find(std::pair<string,string> p);
    Symbol * find(string name);
  protected:
    void makeSymbolTable();
  };

  class OberonModule: public NamedSymbol {
  public:
    Scope * scope;
    OberonModule(const std::string m_name, Scope * m_scope)
      : NamedSymbol(m_name), scope(m_scope) {};
  };

  // Default global types

  class IntegerType: public Type {
  public:
    IntegerType()
      : Type("INTEGER") {};
  public:
    llvm::Value * convertFrom(llvm::Value * v) override;
    llvm::Type * llvmType() override {return llvm::Type::getInt64Ty(*Context);}
  };

  class FloatType: public Type {
  public:
    FloatType()
      : Type("FLOAT") {};
    llvm::Value * convertFrom(llvm::Value * v) override;
    llvm::Type * llvmType() override {return llvm::Type::getDoubleTy(*Context);}
  };



  extern Scope * globalScope;
  extern Func * currentFunc;

  void InitializeGlobalScope();

}




#endif // __SYMBOLS_H__
