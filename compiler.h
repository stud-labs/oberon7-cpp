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

#ifndef __COMPILER_H__
#define __COMPILER_H__

using namespace std;

namespace o7c {

  extern std::unique_ptr<llvm::LLVMContext> Context;
  extern std::unique_ptr<llvm::IRBuilder<>> Builder;
  extern std::unique_ptr<llvm::Module> Module;

  void InitializeModule(const string moduleName);

}

#endif // __COMPILER_H__
