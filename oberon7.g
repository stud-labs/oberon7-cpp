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
#include <string>

using namespace std;
using namespace o7c;
}


ident
   : IDENT
   ;

qualident
   : (ident '.')? ident
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

pointerType
   : POINTER TO type_
   ;

procedureType
   : PROCEDURE formalParameters?
   ;

variableDeclaration locals [Type * t]
   : ids=identList ':'
        {
            $t = NULL;
        } type_
        {
            currentScope->addVariables($ids.ids, $t)
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
    : RETURN expression
//    : RETURN HEXDIGIT
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
   : procedureHeading ';' procedureBody ident
   ;

procedureHeading locals [Params * params]
   : PROCEDURE pid=identdef
        {
            $params = new Params($pid.text, currentScope);
            currentScope = $params;
        }
        formalParameters?
        {
            Func * func = new Func($pid.text, $params, NULL);
            $params->scope->addFunc($pid.text, func);
            currentScope = new Scope($pid.text, $params);
        }
   ;

procedureBody
   : declarationSequence (BEGIN statementSequence)? END
        {
            currentScope->printSymbolTable();
            currentScope = currentScope->scope->scope; // TODO Free Scope *
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

fPSection
   : VAR? ident (',' ident)* ':' formalType
   ;

formalType
   : (ARRAY OF)* qualident
   ;

module returns [o7c::Scope * s]
    : MODULE mid=ident
        {
            $s = new o7c::Scope($mid.text);
        }
        ';' importList?
        declarationSequence (BEGIN statementSequence)?
        END emid=ident
        {
            $mid.text == $emid.text
        }?
        '.' EOF
        {
            currentScope->printSymbolTable();
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
