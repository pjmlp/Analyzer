# Introduction
This is an analyzer for a very small subset of Pascal as described in the grammar section.

It was developed with [SWI Prolog](http://www.swi-prolog.org/) and the grammar was defined with help of [DCG Grammar rules](http://www.swi-prolog.org/pldoc/doc_for?object=section%282,%274.12%27,swi%28%27/doc/Manual/DCG.html%27%29%29).    

# Usage
To use the program you need to consult the file _analyzer.pl_ from the XPCE environment.

Afterwards the _main/1_ predictate needs to be called with the filename to analyze, including the file extension.

If all goes well, yes is returned, otherwise an error message referring to the line where the error ocurred and the token expected is given. 

    C:\analyzer>"c:\Program Files (x86)\swipl\bin\swipl.exe
    Welcome to SWI-Prolog (Multi-threaded, 32 bits, Version 6.2.6)
    Copyright (c) 1990-2012 University of Amsterdam, VU Amsterdam
    SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software,
    and you are welcome to redistribute it under certain conditions.
    Please visit http://www.swi-prolog.org for details.
    
    For help, use ?- help(Topic). or ?- apropos(Word).
    
    1 ?- consult('analyzer.pro').
    % analyzer.pro compiled 0.02 sec, 181 clauses
    true.
    
    2 ?- main('sample.pas').
    true

# Grammar

The subset of Pascal is the following, in which _empty_ denotes an empty production :

    start     ::= PROGRAM ident '('  identList ')' ';' decls 
              subProgDecls compoundStm '.'.

    identList ::= ident, idLists.

    idLists   ::= ',' ident idLists | empty.

        decls     ::= VAR varDecls | empty.

    varDecls  ::= identList ':' type ';' varDecls | empty.

    type      ::= ARRAY '[' intConst '..' intConst ']' OF ident
            | ident.

    subProgDecls ::= subProgDecl  ';' subProgDecls | empty.

    subProgDecl  ::= subProgHead decls compoundStm.

    subProgHead  ::= FUNCTION  ident arguments ':' ident ';' 
               | PROCEDURE ident arguments ';'.

    arguments    ::= '(' parameterList ')'| empty. 

    parameterList  ::= identList ':' type parameterLists.

    parameterLists ::= ';' identList ':' type parameterLists 
                 | empty.

    compoundStm ::= BEGIN optStm END. 

    optStm      ::= stmList | empty.

    stmList     ::= statement, stmLists.

    stmLists    ::= ';' statement stmLists | empty.

    statement ::= ident params
            | ident arrayIndex ':=' exp
            | ident ':=' exp
            | ident
            | compoundStm
            | IF exp THEN statement elsePart
            | WHILE exp DO statement. 


    elsePart ::= ELSE statement | empty.

    variable ::= ident arrayIndex.

    arrayIndex ::= '[' exp ']'. 

    params     ::= ['('], expList, [')'].

    expList    ::= exp expLists.

    expLists   ::= ',' exp expLists | empty.

    exp        ::= simpleExp relExp.

    relExp     ::= relOp simpleExp | empty.

    simpleExp  ::= sign term addExp.

    addExp     ::= addOp term addExp | empty.

    term       ::= factor mulExp.

    mulExp     ::= mulOp factor mulExp | empty.

    factor     ::= intConst
             | ident params
             | ident arrayIndex
             | ident
             | '(' exp ')' 
             | NOT factor.

    sign  ::= '+' | '-' | empty.

    relOp ::= '<' | '<=' | '>' | '>=' | '=' | '<>'.

    addOp ::= '+' | '-' | OR.

    mulOp ::= '*' | '/' | DIV | MOD | AND.

    ident ::= letter nextIdent.

    nextIdent ::= letter nextIdent
            | digit nextIdent
            | empty.

    intConst  ::= digit nextDigit.

    nextDigit ::= digit nextDigit |  empty.

#Bibliography

Formal Syntax and Semantics of Programming Languages

Kenneth Slanneger, Barry L. Kurtz

Addison-Wesley 1995 