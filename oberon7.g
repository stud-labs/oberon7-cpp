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
   | (DIGIT (HEXDIGIT | DIGIT)* 'H')
   ;

real
   : DIGIT+ '.' DIGIT* scaleFactor?
   ;

scaleFactor
   : 'E' ('+' | '-')? DIGIT+
   ;

number
   : integer
   | real
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

procedureType returns [ProcType * procType]
   : PROCEDURE formalParameters? {$procType = NULL;}
   ;

variableDeclaration
   : ids=identList ':'
        t=type_
        {
            currentScope->addVariables($ids.ids, $t.type)
        }?
   ;

expression
   : simpleExpression (relation simpleExpression)?
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

simpleExpression
   : ('+' | '-')? term (addOperator term)*
   ;

addOperator
   : '+'
   | '-'
   | OR
   ;

term
   : factor (mulOperator factor)*
   ;

mulOperator
   : '*'
   | '/'
   | DIV
   | MOD
   | '&'
   ;

factor
   : number
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
   : (assignment | procedureCall | ifStatement | caseStatement | whileStatement | repeatStatement | forStatement | returnStatement )?
   ;

returnStatement
    : RETURN i=integer
        {
            Builder->CreateRet(
                llvm::ConstantInt::get(
                    llvm::IntegerType::get(
                        Module->getContext(), 64),
                    $i.text,
                    10
                )
            );
        }
//    : RETURN expression
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
   : procedureHeading
        {
            // cout << "At heading:" << currentScope->name << " "
            //      << (void *) currentScope
            //      << endl;
        }
        ';' procedureBody id=ident
        {
            // cout << "At end:" << currentScope->name
            //      << " id:" << $id.text << " "
            //      << (void *) currentScope
            //      << endl;
        }
        {
            currentScope->name == $id.text
        }?
        {
            currentScope = currentScope->scope; // TODO: delete prev
        }
   ;

procedureHeading locals [Params * params, llvm::Function * func = NULL]
   : PROCEDURE pid=identdef
        {
            cout << "Params: " << $pid.text << endl;
            $params = new Params($pid.text, currentScope);
            currentScope = $params;
        }
        formalParameters?
        {
            Func * func = new Func($pid.text, $params, NULL);
            $params->scope->addFunc(func->name, func);
            currentScope = new Scope(func->name, $params);
            // cout << "init:" << currentScope->name << " pid:" << $pid.text << endl;

            llvm::FunctionType * FT = llvm::FunctionType::get(
                llvm::Type::getVoidTy(*Context), false
            );
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
   ;

procedureBody
   : declarationSequence (BEGIN statementSequence)? END
        {
            currentScope->printSymbolTable();
            cout<<"\nCODE:\n";
            currentScope->llvmFunc->print(llvm::errs());
            currentScope = currentScope->scope; // TODO Free Scope *
        }
   ;

declarationSequence
    :
        declaration*
    ;

declaration
    :
        CONST (constDeclaration ';')*
    |   TYPE (typeDeclaration ';')*
    |   VAR (variableDeclaration ';')
    |   procedureDeclaration ';'
    ;


formalParameters
   : '(' (fPSection (';' fPSection)*)? ')' (':' qualident)?
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
   | (DIGIT (HEXDIGIT | DIGIT)* 'X')
   ;

HEXDIGIT
   : 'A'
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
