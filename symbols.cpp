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
#include "llvm/IR/Instruction.h"

#include "symbols.h"
#include <algorithm>
#include <ostream>

using namespace std;

namespace o7c {

  Scope * globalScope = nullptr;

  void NamedSymbol::printOn(ostream& os) const {
    os << "Symbol '" << name << "'";
  }

  void Type::printOn(ostream& os) const {
    os << "Type '" << name << "'";
  }

  void Variable::printOn(ostream& os) const {
    os << "Variable '" << name << "':" << type->name;
  }

  Type * Variable::setType(Type * ty) {
    if (type) return nullptr;
    type = ty;
    return ty;
  }

  Symbol * Scope::add(Symbol * s) {

    if (s->hasName()) {
      const NamedSymbol * n = (const NamedSymbol *) s;
      if (symbolTable.find(n->name) == symbolTable.end()) {
        symbolTable[n->name] = s;
      } else {
        std::cerr << s
                  << " already registered in the scope (same name exists)"
                  << std::endl;
        return nullptr;
      }
    } else {
      if (std::find(symbols.begin(), symbols.end(), s)==symbols.end()) {
        std::cerr << s
                  << " already registered in the scope (same object exists)"
                  << std::endl;
        return nullptr;
      }
    }

    symbols.push_back(s);

    return s;
  }

  void Scope::makeSymbolTable() {
    for(Symbol * s: symbols) { // suppose symbols are correct
      if(s->hasName()) {
        const NamedSymbol * n = (const NamedSymbol *) s;
        symbolTable[n->name] = s;
      }
    }
  }

  Symbol * Scope::find(std::pair<string,string> p) {
    Scope * ns = nullptr;
    if (!p.first.empty()) {
      OberonModule * m = (OberonModule *) globalScope->find(p.first);
      ns = m->scope;
    } else ns = this;
    return ns->find(p.second);
  }


  Symbol * Scope::find(string name) {
    auto p = symbolTable.find(name);
    if (p == symbolTable.end()) {
      std::cerr << "Cannot find symbol '" << name << "'\n";
      return nullptr;
    }
    return symbolTable[name];
  }

  llvm::Value * IntegerType::convertFrom(llvm::Value * v) {
    llvm::Type * ty = v->getType();
    if (ty->isIntegerTy()) {
      return v;
    } else {
      std::cerr << "Cannot convert"
                << " to INTEGER\n";
      return nullptr;
    }
  }

  llvm::Value * FloatType::convertFrom(llvm::Value * v) {
    llvm::Type * ty = v->getType();
    if (ty->isDoubleTy()) {
      return v;
    } else {
      // llvm::Value * nv = Builder->CreateFPCast(v, llvmType());
      llvm::Value * nv = Builder->CreateCast(llvm::Instruction::SIToFP, v, llvmType());
      return nv;
    }
  }

  Scope * currentScope = nullptr;
  Func * currentFunc = nullptr;

  void InitializeGlobalScope() {
    Scope * s = new Scope();

    s->add(new IntegerType());
    s->add(new FloatType());

    globalScope = s;
    currentScope = s;
    currentFunc = nullptr;
  }

}
