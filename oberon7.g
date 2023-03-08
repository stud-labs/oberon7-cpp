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
#include "symbols.h"
    #include "compiler.h"
    #include <string>

using namespace std;
    using namespace o7c;
}

ident
   : IDENT
   ;

qualident returns [Qual * q] locals [vector<string> qual]
   : (id=ident
            {
                $qual.push_back($id.text);
            }
            '.')? nid=ident
        {
            $qual.push_back($nid.text);
            $q = new Qual($qual, currentScope);
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

type_ returns [Type * type]
   : q=qualident {$type = new QualType($q.q);}
   | a=arrayType {$type = $a.at;}
   | r=recordType {$type = $r.rec;}
   | p=pointerType {$type = $p.pointer;}
   | c=procedureType {$type = $c.procType;}
   ;

arrayType returns [Array * at] locals [vector<int> dims]
   : ARRAY dp=length
        {
            $dims.push_back($dp.l);
        }
        (',' ndp=length
            {
                $dims.push_back($ndp.l);
            }
        )* OF t=type_
        {
            $at = new Array($dims, $t.type);
        }
   ;

length returns [int l]
   : i=integer { $l=atoi($i.text.c_str()); }
//   : constExpression { $l=100; }
   ;

recordType returns [Record * rec]
   : RECORD ('(' baseType ')')? fieldListSequence? END { $rec=NULL;}
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

identList returns [vector<string> ids]
   : id=identdef
        {
            $ids.push_back($id.text);
        }
        (',' nid=identdef
            {
                $ids.push_back($nid.text);
            }
        )*
   ;

pointerType returns [Pointer * pointer]
   : POINTER TO type_ {$pointer=NULL;}
   ;

procedureType returns [ProcType * procType = NULL]
    : PROCEDURE '(' formalParameters ')' (':' qualident)?
    | PROCEDURE (':' qualident)?
   ;

variableDeclaration
   : ids=identList ':'
        t=type_
        {
            currentScope->addVariables($ids.ids, $t.type)
        }?
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
    : RETURN e=expression
        {
            Builder->CreateRet($e.val);
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
   : head=procedureHeading ';' procedureBody id=ident
        {
            currentScope->name == $id.text
        }?
        {
            currentScope = currentScope->scope; // TODO: delete prev
        }
   ;

procedureHeading returns [llvm::Function * func] locals [Params * params = NULL, llvm::Type * retTy = NULL]
    : PROCEDURE pid=identdef '('
        {
            cout << "Params: " << $pid.text << endl;
            $params = new Params($pid.text, currentScope);
            currentScope = $params;
        }
       formalParameters ')' (':' ty=qualident
            {
                $retTy = llvm::Type::getInt64Ty(*Context); // TODO Not Implemented
            })?
       {
            llvm::ArrayRef<llvm::Type *> args;
            if ($retTy == NULL) {
                $retTy = llvm::Type::getVoidTy(*Context);
            };

            Func * func = new Func($pid.text, $params, NULL);
            $params->scope->addFunc(func->name, func); // 1
            currentScope = new Scope(func->name, $params);
            // cout << "init:" << currentScope->name << " pid:" << $pid.text << endl;

            llvm::FunctionType * FT = llvm::FunctionType::get($retTy, args, false);
            $func = llvm::Function::Create(
                FT, llvm::Function::ExternalLinkage,
                $pid.text,
                *Module);
            llvm::BasicBlock *bb = llvm::BasicBlock::Create(
                Module->getContext(),
                "entry", $func);
            Builder->SetInsertPoint(bb);
            currentScope->setLLVMFunc($func);
        }
    | PROCEDURE
        pid=identdef
            {
                cout << "Params: " << $pid.text << endl;
                $params = new Params($pid.text, currentScope);
                currentScope = $params;
            }
        (':' ty=qualident
            {
                $retTy = llvm::Type::getInt64Ty(*Context); // TODO Not Implemented
            })?
        {
            if ($retTy == NULL) {
                $retTy = llvm::Type::getVoidTy(*Context);
            };

            Func * func = new Func($pid.text, $params, NULL);
            $params->scope->addFunc(func->name, func); //2
            currentScope = new Scope(func->name, $params);

            llvm::FunctionType *FT =
                llvm::FunctionType::get($retTy, false);
            $func = llvm::Function::Create(
                FT, llvm::Function::ExternalLinkage,
                $pid.text, *Module);
            llvm::BasicBlock *bb = llvm::BasicBlock::Create(
                Module->getContext(),
                "entry", $func);
            Builder->SetInsertPoint(bb);
            currentScope->setLLVMFunc($func);
        }   ;

procedureBody
   : declarationSequence (BEGIN statementSequence)? END
        {
            currentScope->printSymbolTable();
            //cout<<"\nCODE:\n";
            //currentScope->llvmFunc->print(llvm::errs());
            currentScope = currentScope->scope; // TODO Free Scope *
        }
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

fPSection locals [vector<string> vars]
   : var=VAR? ind=ident
        {
            $vars.push_back($ind.text);
        }
        (
            ',' nind=ident
            {
                $vars.push_back($nind.text);
            }
        )*
        ':' type=formalType
        {
            currentScope->addVariables($vars, $type.ft, $var.text);
        }
   ;

formalType returns [Type * ft]
   : (ARRAY OF)* qual=qualident {$ft=NULL;}
   ;

module returns [o7c::Scope * s] locals [llvm::Function * iniFunc = NULL]
    : MODULE mid=ident
        {
            InitializeModule($mid.text);
            $s = new o7c::Scope($mid.text);
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
            currentScope->printSymbolTable();

            if ($iniFunc) {
                Builder->CreateRet(llvm::UndefValue::get(
                    llvm::Type::getVoidTy(*Context)));
                llvm::verifyFunction(*$iniFunc);
                cout<< endl << "CODE:\n";
                $iniFunc->print(llvm::errs());
                // Free initFunc
                $iniFunc->eraseFromParent();
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
