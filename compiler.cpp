#include <string>
#include "compiler.h"

namespace o7c {

  std::unique_ptr<llvm::LLVMContext> Context;
  std::unique_ptr<llvm::IRBuilder<>> Builder;
  std::unique_ptr<llvm::Module> Module;

  void InitializeCompiler(const std::string moduleName) {
    Context = std::make_unique<llvm::LLVMContext>();
    Module = std::make_unique<llvm::Module>(moduleName, *Context);
    Builder = std::make_unique<llvm::IRBuilder<>>(*Context);
  }

}
