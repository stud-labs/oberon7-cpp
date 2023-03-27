/*
BSD License

Copyright (c) 2020, Tom Everett
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:

1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
3. Neither the name of Tom Everett nor the names of its contributors
   may be used to endorse or promote products derived from this software
   without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

/*
* https://people.inf.ethz.ch/wirth/Oberon/Oberon07.Report.pdf
*/

grammar oberon7;

@header {
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
    #include "compiler.h"
    #include "symbols.h"
    #include <string>

using namespace std;
    using namespace o7c;
}

ident
   : IDENT
   ;

qualident returns [std::pair<string, string> qual]
   : (sc=ident '.' {$qual.first = $sc.text;})?
        id=ident
        {
            $qual.second = $id.text;
            std::cout << "QUAL:" << $qual.first << "." << $qual.second << "\n";
        }
   ;

identdef
   : ident '*'?
   ;

integer
   : (DIGIT+)
   | (DIGIT (DIGIT | HEXDIGIT)* 'H')
   ;

real
   : DIGIT+ '.' DIGIT* scaleFactor?
   ;

scaleFactor
   : 'E' ('+' | '-')? DIGIT+
   ;

number returns [llvm::Value * val]
   : i=integer
        {
            $val = llvm::ConstantInt::get(
                llvm::IntegerType::get(
                    Module->getContext(), 64
                ),
                $i.text,
                10
            );
        }
   | f=real
        {
            float numVal = strtod($f.text.c_str(), nullptr);
            $val = llvm::ConstantFP::get(
                Module->getContext(),
                llvm::APFloat(numVal)
            );
        }
   ;

constDeclaration
   : identdef '=' constExpression
   ;

constExpression
   : expression
   ;

typeDeclaration
   : identdef '=' type_
   ;

type_
   : qualident
   | arrayType
   | recordType
   | pointerType
   | procedureType
   ;

arrayType
   : ARRAY length (',' length)* OF type_
   ;

length
   : constExpression
   ;

recordType
   : RECORD ('(' baseType ')')? fieldListSequence? END
   ;

baseType
   : qualident
   ;

fieldListSequence
   : fieldList (';' fieldList)*
   ;

fieldList
   : identList ':' type_
   ;

identList
   : identdef (',' identdef)*
   ;

pointerType
   : POINTER TO type_
   ;

procedureType
    : PROCEDURE '(' formalParameters ')' (':' qualident)?
    | PROCEDURE (':' qualident)?
    ;

variableDeclaration
   : identList ':' type_
   ;

expression returns [llvm::Value * val = NULL] locals [llvm::Value * val2 = NULL]
   : s=simpleExpression (relation simpleExpression)?
        {
            $val = $s.val; // TODO
        }
   ;

relation
   : '='
   | '#'
   | '<'
   | '<='
   | '>'
   | '>='
   | IN
   | IS
   ;

simpleExpression returns [llvm::Value * val = NULL] locals [llvm::Value * val2 = NULL]
   : ('+' | '-')? t=term (addOperator term)*
        {
            $val = $t.val; // TODO: ...
        }
   ;

addOperator
   : '+'
   | '-'
   | OR
   ;

term returns [llvm::Value * val = NULL] locals [llvm::Value * val2 = NULL]
   : f=factor (mulOperator f2=factor)*
        {
            $val = $f.val; // TODO: Process operator
        }
   ;

mulOperator
   : '*'
   | '/'
   | DIV
   | MOD
   | '&'
   ;

factor returns [llvm::Value * val = NULL]
   : i=number { $val = $i.val; }
   | STRING
   | NIL
   | TRUE
   | FALSE
   | set_
   | designator (actualParameters)?
   | '(' expression ')'
   | '~' factor
   ;

designator
   : qualident selector*
   ;

selector
   : '.' ident
   | '[' expList ']'
   | '^'
   | '(' qualident ')'
   ;

set_
   : '{' (element (',' element)*)? '}'
   ;

element
   : expression ('..' expression)?
   ;

expList
   : expression (',' expression)*
   ;

actualParameters
   : '(' expList? ')'
   ;

statement
    : (assignment
        | procedureCall
        | ifStatement
        | caseStatement
        | whileStatement
        | repeatStatement
        | forStatement
        | returnStatement
      )?
    ;

returnStatement
    : RETURN
        {
            currentFunc != nullptr
        }?
        e=expression
        {
            Type * rt = currentFunc->type;
            llvm::Value * v = rt->convertFrom($e.val);
            // llvm::Value * v = Builder->CreateAdd($e.val, Builder->getInt64(1));
            Builder->CreateRet(v);
        }
   ;

assignment
   : designator ':=' expression
   ;

procedureCall
   : designator actualParameters?
   ;

statementSequence
   : statement (';' statement)*
   ;

ifStatement
   : IF expression THEN statementSequence (ELSIF expression THEN statementSequence)* (ELSE statementSequence)? END
   ;

caseStatement
   : CASE expression OF case_ ('|' case_)* END
   ;

case_
   : (caseLabelList ':' statementSequence)?
   ;

caseLabelList
   : labelRange (',' labelRange)*
   ;

labelRange
   : label ('..' label)?
   ;

label
   : integer
   | STRING
   | qualident
   ;

whileStatement
   : WHILE expression DO statementSequence (ELSIF expression DO statementSequence)* END
   ;

repeatStatement
   : REPEAT statementSequence UNTIL expression
   ;

forStatement
   : FOR ident ':=' expression TO expression (BY constExpression)? DO statementSequence END
   ;

procedureDeclaration
    : head=procedureHeading ';' procedureBody
        {
            currentScope = currentScope->parent;
        }
        id=ident
    ;

procedureHeading returns [llvm::Function * func] locals [llvm::Type * retTy = nullptr, Func * f]
    : PROCEDURE pid=identdef '('
        {
            Scope * fs = new Scope();
            $f = new Func($pid.text, nullptr, fs);
            currentFunc = $f;
            currentScope = fs;
            cout << "Params: " << $pid.text << endl;
        }
       formalParameters ')' (':' q=qualident
            {
                Type * ty = (Type *) currentScope->parent->find($q.qual);
                // TODO: Check whether ty is a type....
                $retTy = ty->llvmType();
                $f->setType(ty);
            })?
        {
            llvm::ArrayRef<llvm::Type *> args;
            if ($retTy == nullptr) {
                $retTy = llvm::Type::getVoidTy(*Context);
            };

            llvm::FunctionType * FT = llvm::FunctionType::get($retTy, args, false);
            $func = llvm::Function::Create(
                FT, llvm::Function::ExternalLinkage,
                $pid.text,
                *Module);
            llvm::BasicBlock *bb = llvm::BasicBlock::Create(
                Module->getContext(),
                "entry", $func);
            Builder->SetInsertPoint(bb);
        }
    | PROCEDURE
        pid=identdef
        {
            Scope * fs = new Scope();
            $f = new Func($pid.text, nullptr, fs);
            currentFunc = $f;
            currentScope = fs;
            cout << "Params: " << $pid.text << endl;
        }
        (':' q=qualident
            {
                Type * ty = (Type *) currentScope->parent->find($q.qual);
                // TODO: Check whether ty is a type....
                $retTy = ty->llvmType();
                $f->setType(ty);
            })?
        {
            if ($retTy == NULL) {
                $retTy = llvm::Type::getVoidTy(*Context);
            };

            llvm::FunctionType *FT =
                llvm::FunctionType::get($retTy, false);
            $func = llvm::Function::Create(
                FT, llvm::Function::ExternalLinkage,
                $pid.text, *Module);
            llvm::BasicBlock *bb = llvm::BasicBlock::Create(
                Module->getContext(),
                "entry", $func);
            Builder->SetInsertPoint(bb);
        }   ;

procedureBody
   : declarationSequence (BEGIN statementSequence)? END
        //        llvm::verifyFunction(*$iniFunc);
   ;

declarationSequence
   : ( declaration )*
   ;

declaration
    :
        CONST (constDeclaration ';')*
    |   TYPE (typeDeclaration ';')*
    |   VAR (variableDeclaration ';')*
    |   procedureDeclaration ';'
    ;

formalParameters
    : (fPSection (';' fPSection)*)?
    ;


fPSection
   : VAR? ident (',' ident)* ':' formalType
   ;

formalType
   : (ARRAY OF)* qualident
   ;

module locals [llvm::Function * iniFunc = NULL]
    : MODULE mid=ident
        {
            InitializeModule($mid.text);
            InitializeGlobalScope();
        }
        ';' importList?
        declarationSequence
        (
            BEGIN
        {
                llvm::FunctionType * FT = llvm::FunctionType::get(
                    llvm::Type::getVoidTy(*Context), false);
                $iniFunc = llvm::Function::Create(
                    FT,
                    llvm::Function::ExternalLinkage,
                    "@INIT@",
                    *Module
                );
                llvm::BasicBlock *BB = llvm::BasicBlock::Create(
                    Module->getContext(),
                    "entry", $iniFunc);
                Builder->SetInsertPoint(BB);
            }
            statementSequence
        )?
        END emid=ident
        {
            $mid.text == $emid.text
        }?
        '.' EOF
        {
            if ($iniFunc) {
                Builder->CreateRet(llvm::UndefValue::get(
                    llvm::Type::getVoidTy(*Context)));
                llvm::verifyFunction(*$iniFunc);
            }
            Module->print(llvm::errs(), nullptr);
            // Module->eraseFromParent();
        }
   ;

importList
   : IMPORT import_ (',' import_)* ';'
   ;

import_
   : ident (':=' ident)?
   ;

ARRAY
   : 'ARRAY'
   ;

OF
   : 'OF'
   ;

END
   : 'END'
   ;

POINTER
   : 'POINTER'
   ;

TO
   : 'TO'
   ;

RECORD
   : 'RECORD'
   ;

PROCEDURE
   : 'PROCEDURE'
   ;

IN
   : 'IN'
   ;

IS
   : 'IS'
   ;

OR
   : 'OR'
   ;

DIV
   : 'DIV'
   ;

MOD
   : 'MOD'
   ;

NIL
   : 'NIL'
   ;

TRUE
   : 'TRUE'
   ;

FALSE
   : 'FALSE'
   ;

IF
   : 'IF'
   ;

THEN
   : 'THEN'
   ;

ELSIF
   : 'ELSIF'
   ;

ELSE
   : 'ELSE'
   ;

CASE
   : 'CASE'
   ;

WHILE
   : 'WHILE'
   ;

DO
   : 'DO'
   ;

REPEAT
   : 'REPEAT'
   ;

UNTIL
   : 'UNTIL'
   ;

FOR
   : 'FOR'
   ;

BY
   : 'BY'
   ;

BEGIN
   : 'BEGIN'
   ;

RETURN
   : 'RETURN'
   ;

CONST
   : 'CONST'
   ;

TYPE
   : 'TYPE'
   ;

VAR
   : 'VAR'
   ;

MODULE
   : 'MODULE'
   ;

IMPORT
   : 'IMPORT'
   ;

STRING
   : ('"' .*? '"')
   | (DIGIT (DIGIT | HEXDIGIT)* 'X')
   ;

HEXDIGIT
   :
     'A'
   | 'B'
   | 'C'
   | 'D'
   | 'E'
   | 'F'
   ;

IDENT
   : LETTER (LETTER | DIGIT)*
   ;

LETTER
   : [a-zA-Z_]
   ;

DIGIT
   : [0-9]
   ;

COMMENT
   : '(*' .*? '*)' -> skip
   ;

WS
   : [ \t\r\n] -> skip
   ;
