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
#include "compiler.h"

using namespace std;

namespace o7c {

  void InitializeModule() {
    Context = std::make_unique<llvm::LLVMContext>();
    Module = std::make_unique<llvm::Module>("OBERON7MODULE", *Context);
    Builder = std::make_unique<llvm::IRBuilder<>>(*Context);
  }


}
