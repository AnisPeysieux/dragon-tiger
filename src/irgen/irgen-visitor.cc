#include <cstdlib>  // For exit
#include <iostream> // For std::cerr
#include "irgen.hh"

#include "llvm/Support/raw_ostream.h"

namespace {

// This function can be removed once the lab has been fully implemented.
//[[noreturn]] void UNIMPLEMENTED() {
//  std::cerr << "Error: unimplemented feature\n";
//  exit(1);
//}

} // namespace

namespace irgen {

llvm::Value *IRGenerator::visit(const IntegerLiteral &literal) {
//  std::cout << "IntegetLiteral: " << literal.value << std::endl;
  return Builder.getInt32(literal.value);
}

llvm::Value *IRGenerator::visit(const StringLiteral &literal) {
  return Builder.CreateGlobalStringPtr(literal.value.get());
}

llvm::Value *IRGenerator::visit(const Break &b) {
  //std::cout << "break" << std::endl;
  //std::cout << "b.get_loop() = " << b.get_loop() << std::endl;
  //std::cout << "&(b.get_loop().get()) = " << &(b.get_loop().get()) << std::endl;
  Builder.CreateBr(loop_exit_bbs[&(b.get_loop().get())]);
  return nullptr;
  //UNIMPLEMENTED();
}

llvm::Value *IRGenerator::visit(const BinaryOperator &op) {
  llvm::Value *l = op.get_left().accept(*this);
  llvm::Value *r = op.get_right().accept(*this);

  if (op.get_left().get_type() == t_string) {
    auto const strcmp = Mod->getOrInsertFunction(
        "__strcmp", Builder.getInt32Ty(), Builder.getInt8PtrTy(),
        Builder.getInt8PtrTy(), nullptr);
    l = Builder.CreateCall(strcmp, {l, r});
    r = Builder.getInt32(0);
  }

  switch(op.op) {
    case o_plus: return Builder.CreateBinOp(llvm::Instruction::Add, l, r);
    case o_minus: return Builder.CreateBinOp(llvm::Instruction::Sub, l, r);
    case o_times: return Builder.CreateBinOp(llvm::Instruction::Mul, l, r);
    case o_divide: return Builder.CreateBinOp(llvm::Instruction::SDiv, l, r);
    default: break;
  }

  // Comparisons return an i1 result which needs to be
  // casted to i32, as Tiger might use that as an integer.
  llvm::Value *cmp;

  switch(op.op) {
    case o_eq: cmp = Builder.CreateICmpEQ(l, r); break;
    case o_neq: cmp = Builder.CreateICmpNE(l, r); break;
    case o_gt: cmp = Builder.CreateICmpSGT(l, r); break;
    case o_lt: cmp = Builder.CreateICmpSLT(l, r); break;
    case o_ge: cmp = Builder.CreateICmpSGE(l, r); break;
    case o_le: cmp = Builder.CreateICmpSLE(l, r); break;
    default: assert(false); __builtin_unreachable();
  }

  return Builder.CreateIntCast(cmp, Builder.getInt32Ty(), true);
}

llvm::Value *IRGenerator::visit(const Sequence &seq) {
  llvm::Value *result = nullptr;
  for (auto expr : seq.get_exprs())
    result = expr->accept(*this);
  // An empty sequence should return () but the result
  // will never be used anyway, so nullptr is fine.
  return result;
}

llvm::Value *IRGenerator::visit(const Let &let) {
  for (auto decl : let.get_decls())
    decl->accept(*this);

  return let.get_sequence().accept(*this);
}

llvm::Value *IRGenerator::visit(const Identifier &id) {
  const VarDecl* decl = &(id.get_decl().get());
  llvm::Value* var = allocations[decl];
  //std::cout << "\tallocations = " << std::endl;
  for(auto it = allocations.cbegin(); it != allocations.cend(); ++it)
  {
      //std::cout << "\t\t"  << it->first->name << " " << it->second << " " << "\n";
  }
  //std::cout << "var = " << var << std::endl;
  llvm::Value* load = Builder.CreateLoad(var);
  return load;
  //UNIMPLEMENTED();
}

llvm::Value *IRGenerator::visit(const IfThenElse &ite) {
  //std::cout << "IfThenElse" << std::endl;

  llvm::Value* result = nullptr;
  if(ite.get_type() != t_void)
  {
    result = alloca_in_entry(llvm_type(ite.get_type()), "if_result");
  }
  //llvm::Value* const result = alloca_in_entry(Builder.getInt1Ty(), "if_result");
  
  llvm::BasicBlock* const then_block = llvm::BasicBlock::Create(Context, "if_then", current_function);
  llvm::BasicBlock* const else_block = llvm::BasicBlock::Create(Context, "if_else", current_function);
  llvm::BasicBlock* const end_block = llvm::BasicBlock::Create(Context, "if_end", current_function);
  
  Builder.CreateCondBr(
                        Builder.CreateIsNotNull(ite.get_condition().accept(*this)),
                        then_block,
                        else_block);
  
  Builder.SetInsertPoint(then_block);
  llvm::Value* const then_result = ite.get_then_part().accept(*this);
  //if(then_result != nullptr)
  if(ite.get_type() != t_void)
  {
    //std::cout << "then no nullptr" << std::endl;
    Builder.CreateStore(then_result, result);
  }
  else
  {
    //std::cout << "then nullptr" << std::endl;
  }
  Builder.CreateBr(end_block);

  Builder.SetInsertPoint(else_block);
  llvm::Value* const else_result = ite.get_else_part().accept(*this);
  //if(else_result != nullptr)
  if(ite.get_type() != t_void)
  {
    //std::cout << "else no nullptr" << std::endl;
    Builder.CreateStore(else_result, result);
  }
  else
  {
    //std::cout << "else nullptr" << std::endl;
  }
  Builder.CreateBr(end_block);

  Builder.SetInsertPoint(end_block);
  if(ite.get_type() != t_void)
  {
    return Builder.CreateLoad(result);
  }
  else
  {
    return nullptr;
  }
 
 
  //UNIMPLEMENTED();
}

llvm::Value *IRGenerator::visit(const VarDecl &decl) {
  //std::cout << "VarDecl " << decl.name << std::endl;
  llvm::Value* var = alloca_in_entry(llvm_type(decl.get_type()), decl.name);
  allocations.insert(std::pair<const VarDecl*, llvm::Value*>(&decl, var));
 // std::cout << "\tallocations = " << std::endl;
 // for(auto it = allocations.cbegin(); it != allocations.cend(); ++it)
 // {
 //     std::cout << "\t\t"  << it->first->name << " " << it->second << " " << "\n";
 // }
  const Expr* e = &(decl.get_expr().get());
  llvm::Value* ret_expr = e->accept(*this);
  Builder.CreateStore(ret_expr, var);
//  UNIMPLEMENTED();
  //return var;
  return var;
}

llvm::Value *IRGenerator::visit(const FunDecl &decl) {
  //std::cout << "FunDecl " << decl.name << std::endl;
  std::vector<llvm::Type *> param_types;

  for (auto param_decl : decl.get_params()) {
    param_types.push_back(llvm_type(param_decl->get_type()));
  }

  llvm::Type *return_type = llvm_type(decl.get_type());

  llvm::FunctionType *ft =
      llvm::FunctionType::get(return_type, param_types, false);

  llvm::Function::Create(ft,
                         decl.is_external ? llvm::Function::ExternalLinkage
                                          : llvm::Function::InternalLinkage,
                         decl.get_external_name().get(), Mod.get());

  if (decl.get_expr())
{
    //std::cout << "\tpending" << std::endl;
    pending_func_bodies.push_front(&decl);
}
  //std::cout << "EndFunDecl " << decl.name << std::endl;
  return nullptr;
}

llvm::Value *IRGenerator::visit(const FunCall &call) {
  // Look up the name in the global module table.
  //std::cout << "FunCall " << call.get_decl().get().get_external_name().get() << std::endl;
  const FunDecl &decl = call.get_decl().get();
  llvm::Function *callee =
      Mod->getFunction(decl.get_external_name().get());

  if (!callee) {
    // This should only happen for primitives whose Decl is out of the AST
    // and has not yet been handled
    assert(!decl.get_expr());
    decl.accept(*this);
    callee = Mod->getFunction(decl.get_external_name().get());
  }

  std::vector<llvm::Value *> args_values;
  for (auto expr : call.get_args()) {
    args_values.push_back(expr->accept(*this));
  }

  if (decl.get_type() == t_void) {
    Builder.CreateCall(callee, args_values);
  //std::cout << "EndFunCall void " << call.get_decl().get().get_external_name().get() << std::endl;
    return nullptr;
  }
  //std::cout << "EndFunCall no void " << call.get_decl().get().get_external_name().get() << std::endl;
  return Builder.CreateCall(callee, args_values, "call");
}

llvm::Value *IRGenerator::visit(const WhileLoop &loop) {
  //std::cout << "WhileLoop" << std::endl;
  
  llvm::BasicBlock* const cond_loop_block = llvm::BasicBlock::Create(Context, "cond_loop", current_function);
  llvm::BasicBlock* const body_loop_block = llvm::BasicBlock::Create(Context, "body_loop", current_function);
  llvm::BasicBlock* const end_loop_block = llvm::BasicBlock::Create(Context, "end_loop", current_function);
  //std::cout << "While: &loop = " << &loop << std::endl;
  loop_exit_bbs[&loop] = end_loop_block;

  
  Builder.CreateBr(cond_loop_block);
  Builder.SetInsertPoint(cond_loop_block);
  Builder.CreateCondBr(
                        Builder.CreateIsNotNull(loop.get_condition().accept(*this)),
                        body_loop_block,
                        end_loop_block);
  Builder.SetInsertPoint(body_loop_block);
  //llvm::Value* const body_result = loop.get_body().accept(*this);
  loop.get_body().accept(*this);
  Builder.CreateBr(cond_loop_block);
  Builder.SetInsertPoint(end_loop_block);
  return nullptr;

  //UNIMPLEMENTED();
}

llvm::Value *IRGenerator::visit(const ForLoop &loop) {
  llvm::BasicBlock *const test_block =
      llvm::BasicBlock::Create(Context, "loop_test", current_function);
  llvm::BasicBlock *const body_block =
      llvm::BasicBlock::Create(Context, "loop_body", current_function);
  llvm::BasicBlock *const end_block =
      llvm::BasicBlock::Create(Context, "loop_end", current_function);
  llvm::Value *const index = loop.get_variable().accept(*this);
  llvm::Value *const high = loop.get_high().accept(*this);
  Builder.CreateBr(test_block);

  Builder.SetInsertPoint(test_block);
  Builder.CreateCondBr(Builder.CreateICmpSLE(Builder.CreateLoad(index), high),
                       body_block, end_block);

  Builder.SetInsertPoint(body_block);
  loop.get_body().accept(*this);
  Builder.CreateStore(
      Builder.CreateAdd(Builder.CreateLoad(index), Builder.getInt32(1)), index);
  Builder.CreateBr(test_block);

  Builder.SetInsertPoint(end_block);
  
  loop_exit_bbs[&loop] = end_block;
  
  return nullptr;
}

llvm::Value *IRGenerator::visit(const Assign &assign) {
  //llvm::Value* identifier = assign.get_lhs().accept(*this);
  //std::cout << "Assign " << assign.get_lhs().get_decl().get().name << std::endl;
  //const VarDecl* vardecl = &(assign.get_lhs().get_decl().get());
  //llvm::Value* var = allocations[vardecl];
  llvm::Value* var = address_of(assign.get_lhs());
 // std::cout << "\tallocations = " << std::endl;
 // for(auto it = allocations.cbegin(); it != allocations.cend(); ++it)
 // {
 //     std::cout << "\t\t"  << it->first->name << " " << it->second << " " << "\n";
 // }
  llvm::Value* ret_expr = assign.get_rhs().accept(*this);
  llvm::Value* store = Builder.CreateStore(ret_expr, var);
  return store;
  //UNIMPLEMENTED();
}

} // namespace irgen
