/* analyzer.pl - Syntatic analyzer of a small subset of Pascal.
 * Copyright (C) 1997  Paulo Pinto, Pablo Tavares
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */


start --> ([(program,_,_)];
           {expected(identifier, L, C)}), 
          (ident;
           [(_, L, C)], {expected(identifier, L, C)}),        % Missing an identifier
          ([('(',_,_)];
           [(_, L, C)], {expected('(', L, C)}),               % '(' Missing
          identList, 
          ([(')',_,_)];
           [(_, L, C)], {expected(')', L, C)}),               % ')' Missing
          ([(';',_,_)];
           [(_, L, C)], {expected(';', L, C)}),               % ';' Missing
          decls, 
          subProgDecls, 
          compoundStm,
          ([('.',_,_)];
           [(_, L, C)], {expected('.', L, C)}),               % '.' Missing
          [(eof,_,_)].

% Identifiers List

identList --> ident, idLists.

idLists --> [(',',_,_)], ident, idLists.
idLists --> [].


% Variable Declarations

decls --> [(var,_,_)], varDecls.
decls --> [].

varDecls --> identList,
             ([(':',_,_)];
              [(_, L, C)], {expected(':', L, C)}),             %  ':' Missing
             (type;
              [(_, L, C)], {expected(type, L, C)}),            %  Type Missing
             ([(';',_,_)];
              [(_, L, C)], {expected(';', L, C)}),             %  ';' Missing
             varDecls.
varDecls --> [].

% Types

type --> [(array,_,_)], 
         ([('[',_,_)];
          [(_, L, C)], {expected('[', L, C)}),                %  '[' Missing      
         (intConst;
          [(_, L, C)], {expected('integer constant', L, C)}), %  intConst Expected
         ([('..',_,_)];
          [(_, L, C)], {expected('..', L, C)}),               %  '..' Missing
         (intConst;
          [(_, L, C)], {expected('integer constant', L, C)}), %  intConst Expected          
         ([(']',_,_)];
          [(_, L, C)], {expected(']', L, C)}),                %  ']' Missing
         ([(of,_,_)];
          [(_, L, C)], {expected(of, L, C)}),                 %  'of' Missing
         (ident;
          [(_, L, C)], {expected(indentifier, L, C)}).        %  ident Missing

type --> ident.

% Procedures and functions

subProgDecls --> subProgDecl,
                 ([(';',_,_)];
                  [(_, L, C)], {expected(';', L, C)}),       %  ';' Missing
                  subProgDecls.
subProgDecls --> [].

subProgDecl --> subProgHead, decls, compoundStm.

subProgHead --> [(function,_,_)], 
                (ident;
                 [(_, L, C)], {expected(indentifier, L, C)}),  %  ident Missing
                arguments,
                ([(':',_,_)];
                 [(_, L, C)], {expected(':', L, C)}),          %  ':' Missing
                (ident;
                 [(_, L, C)], {expected(indentifier, L, C)}),  %  ident Missing
                ([(';',_,_)];
                 [(_, L, C)], {expected(';', L, C)}).          %  ';' Missing

subProgHead --> [(procedure,_,_)], 
                (ident;
                 [(_, L, C)], {expected(indentifier, L, C)}),  %  ident Missing
                arguments, 
                ([(';',_,_)];
                 [(_, L, C)], {expected(';', L, C)}).          %  ';' Missing


arguments --> [('(',_,_)], 
              parameterList,
              ([(')',_,_)];
               [(_, L, C)], {expected(')', L, C)}).         %  ')'  Missing

arguments --> [].


parameterList --> identList,
                  ([(':',_,_)];
                   [(_, L, C)], {expected(':', L, C)}),     %  ':' Missing
                  (type;
                   [(_, L, C)], {expected(type, L, C)}),    %  Type Missing
                  parameterLists.

parameterLists --> [(';',_,_)],
                   identList,
                   ([(':',_,_)];
                    [(_, L, C)], {expected(':', L, C)}),    %  ':' Missing
                   (type;
                    [(_, L, C)], {expected(type, L, C)}),    %  Type Missing
                   parameterLists.
parameterLists --> [].

% Compound Statements

compoundStm --> [(begin,_,_)],
                optStm,
                ([(end,_,_)];
                 [(_, L, C)], {expected(end, L, C)}).        %  end  Missing

optStm --> stmList.
optStm --> [].

stmList --> statement, stmLists.

stmLists --> [(';',_,_)], statement, stmLists.
stmLists --> [].

% Statements

statement --> ident,
              params.
statement --> ident,
              arrayIndex,
              ([(':=',_,_)];
               [(_, L, C)], {expected(':=', L, C)}),                        %  ':=' Missing
              exp.
statement --> ident,
              [(':=',_,_)],
              exp.
statement --> ident.
statement --> compoundStm.
statement --> [(if,_,_)], 
              exp,
              ([(then,_,_)];
               [(_, L, C)], {expected(then, L, C)}),                        %  then Missing     
              (statement;
               [(_,L,C)], {expected('identifier , if, begin, while',L, C)}), % Bad instruction 
              elsePart.
statement --> [(while,_,_)],
              exp,
              ([(do,_,_)];
               [(_, L, C)], {expected(do, L, C)}),                           %  do Missing 
              (statement;
               [(_,L,C)], {expected('identifier , if, begin, while',L, C)}).  % Bad instruction 



elsePart --> [(else,_,_)], statement.
elsePart --> [].

variable --> ident, arrayIndex.

arrayIndex --> [('[',_,_)],
               exp,
               ([(']',_,_)];
                [(_, L, C)], {expected(']', L, C)}).         %  ')'  Missing

params --> [('(',_,_)],
           expList,
           ([(')',_,_)];
            [(_, L, C)], {expected(')', L, C)}).             %  ')' Missing


% Expressions

expList --> exp, expLists.

expLists --> [(',',_,_)], exp, expLists.
expLists --> [].

exp --> simpleExp, relExp.

relExp --> relOp, simpleExp.
relExp --> [].

simpleExp --> sign, term, addExp.

addExp --> addOp, term, addExp.
addExp --> [].

term --> factor, mulExp.

mulExp --> mulOp, factor, mulExp.
mulExp --> [].

factor --> intConst.
factor --> ident, params.
factor --> ident, arrayIndex.
factor --> ident.
factor --> [('(',_,_)],
           exp,
           ([(')',_,_)];
            [(_, L, C)], {expected(')', L, C)}).               %  ')' 
factor --> [(not,_,_)], factor.
factor --> [(_, L, C)], {expected('ident, intConst, not, (',L, C)}.  % Bad instruction 

sign --> [('+',_,_)].
sign --> [('-',_,_)].
sign --> [].

% Relational Operators

relOp --> [('<',_,_)].
relOp --> [('<=',_,_)].
relOp --> [('>',_,_)].
relOp --> [('>=',_,_)].
relOp --> [('=',_,_)].
relOp --> [('<>',_,_)].


% Addictive Operators

addOp --> [('+',_,_)].
addOp --> [('-',_,_)].
addOp --> [(or,_,_)].

% Multiplier Operators

mulOp --> [('*',_,_)].
mulOp --> [('/',_,_)].
mulOp --> [(div,_,_)].
mulOp --> [(mod,_,_)].
mulOp --> [(and,_,_)].

% Identifiers

ident --> [(id(_),_,_)].

% Numbers

intConst --> [(num(_),_,_)].

/*************************************************************************************************/

% Scannig



% Keywords
% keyword (-key)

keyword(and).
keyword(array).
keyword(begin).
keyword(div).
keyword(do).
keyword(else).
keyword(end).
keyword(function).
keyword(if).
keyword(mod).
keyword(not).
keyword(of).
keyword(or).
keyword(procedure).
keyword(program).
keyword(then).
keyword(while).
keyword(var).


% Tells if Code represents a single char
% single (+Code, -Token)

single(40, '(').
single(41, ')').
single(42, *).
single(43, +).
single(44, ',').
single(45, -).
single(47, /).
single(59, ';').
single(61, '=').
single(91, '[').
single(93, ']').

% Tells if Code represents a char that precedes a two char token
% double (+Code, -Token)

double(46, '.').
double(58, ':').
double(60, '<').
double(62, '>').

% Tells if Code1 and Code2 represent  a two char token
% pair (+Code, +Code2, -Token)

pair(46, 46, '..').
pair(58, 61, ':=').
pair(60, 61, '<=').
pair(60, 62, '<>').
pair(62, 61, '>=').

% Have we reached the end of file ?
% atEnd (+C)

atEnd(-1).


% Tells if it's a digit
% isDigit (+C)

isDigit(C) :- 48 =< C, C =< 57.

% Tells if it's a letter
% isLetter (+C)

isLetter(C) :- 97 =< C, C =< 122.
isLetter(C) :- 65 =< C, C =< 90.

% Tells if it's a space
% isSpace (+C)

isSpace(32).
isSpace(9).

% Tells if it's a new line
% isNewLine(+C)

isNewLine(10).

% Changes a char from upper case to lower case
% toLower (+C, -NewC)

toLower(C, NewC) :- C >= 65, C =< 90, NewC is C + 32.
toLower(C, C).


% Is C allowed in an identifier
% isIdChar (+C)

isIdChar(C) :- isDigit(C).
isIdChar(C) :- isLetter(C).

% Reads a char
% getChar(-C)

getChar(C) :-  get0(Char),
               toLower(Char, C),
               !.


% Reads a token
% getToken (+Char, +Line, +Col, -Token, -NextChar, -NLine, -NCol)

getToken(Char, Line, Col, (num(N), Line, Col), NextChar, Line, NCol) :- 
                                   isDigit(Char),
                                   getChar(TempCh),
                                   restNum(TempCh, ChList, NextChar),
                                   name(N, [Char|ChList]),
                                   length([Char|ChList],X),
                                   NCol is Col + X .

getToken(Char, Line, Col, (Token, Line, Col), NextChar, Line, NCol) :- 
                                   isLetter(Char),
                                   getChar(TempCh),
                                   restId(TempCh, ChList, NextChar),
                                   name(Id, [Char|ChList]),
                                   (keyword(Id), Token = Id; Token = id(Id)),
                                   length([Char|ChList],X),
                                   NCol is Col + X .


getToken(Char, Line, Col, (Token, Line, Col), NextChar, Line, NCol) :- 
                                   single(Char, Token),
                                   getChar(NextChar),
                                   NCol is Col + 1.

getToken(Char, Line, Col, (Token, Line, Col), NextChar, Line, NCol) :- 
                                   double(Char, TempTok),
                                   getChar(TempCh),
                                   (pair(Char, TempCh, Token), getChar(NextChar), NCol is Col + 2;
                                    Token = TempTok, NextChar = TempCh, NCol is Col + 1).


getToken(Char, Line, Col, (eof, Line, Col), 0, Line, Col) :- 
                                   atEnd(Char).

getToken(Char, Line, Col, Token, NextChar, NLine, NCol) :- 
                                   isSpace(Char),
                                   getChar(TempCh),
                                   TCol is Col + 1,
                                   getToken(TempCh, Line, TCol, Token, NextChar, NLine, NCol).

getToken(Char, Line, _, Token, NextChar, NLine, NCol) :- 
                                   isNewLine(Char),
                                   getChar(TempCh),
                                   TCol is 1,
                                   TLine is Line + 1,
                                   getToken(TempCh, TLine, TCol, Token, NextChar, NLine, NCol).

getToken(Char, Line, Col, _, _, _, _):- unknownChar(Char, Line, Col), abort.


% Reads the remaining of a number
% restNum (+Char, -ChList, -NextChar)

restNum(Char, [Char|ChList], NextChar) :- isDigit(Char),
                                          getChar(TempCh),
                                          restNum(TempCh, ChList, NextChar).

restNum(Char, [], Char).

% Reads the remaining of an identifier
% restId (+Char, -ChList, -NextChar)

restId(Char, [Char|ChList], NextChar) :- isIdChar(Char),
                                         getChar(TempCh),
                                         restId(TempCh, ChList, NextChar).

restId(Char, [], Char).

% Reads the Pascal file
% readFile (-List)

readFile([Token|T]) :- getChar(C),
                       getToken(C, 1, 1, Token,NextCh, Line, Col),
                       restFile(Token, Line, Col, NextCh, T),
                       !.

% Reads the remaining of the file
% restFile (+Token, +Line, +Col, +Char, -List)

restFile((eof, _, _),    _,   _,   _, []).
restFile(_          , Line, Col,Char, [TempTok|List]) :- 
                                         getToken(Char, Line, Col, TempTok, NextCh, NLine, NCol),
                                         restFile(TempTok, NLine, NCol, NextCh, List). 

% Error message for unknown characters
% unknownChar(+Char, +Line, +Col)

unknownChar(Char, Line, Col) :- nl, write('Error in line '), 
                                write(Line), 
                                write(', col '), 
                                write(Col), 
                                write(' :'), nl,
                                write('Unknown Char: '),
                                put(Char), nl, nl.


% Gives an error message for the token
% expected (+Token, +Line, +Col)

expected(Token, Line, Col) :- nl, write('Error in line '), 
                             write(Line), 
                             write(', col '), 
                             write(Col), 
                             write(' :'), nl,
                             write('Expected : '),
                             write(Token), nl, nl,
                             abort.

% Main predicate
% main (+File)

main(File) :- see(File), readFile(List), seen, start(List, []), nl.







