Grammar:<br>
'start' is the start symbol, all symbols written in CAPS are terminals, other are non-terminals , defined in the ```rational.grm``` file
```
  start : blk 
  blk : dec cmdseq 
  dec : vardec procdec 

  vardec : ratvardec intvardec boolvardec 
  ratvardec : ε
            | RAT varlist SEMI      
  intvardec : ε
            | INT varlist SEMI     
  boolvardec : ε
            | BOOL varlist SEMI    
  varlist : IDENTIFIER              
          | IDENTIFIER COMMA varlist   
  
  procdec : 
          | procdef SEMI procdec      
  procdef : PROC IDENTIFIER blk      

  cmdseq : LBRACE cmds RBRACE 
  cmds : ε
       | cmd SEMI cmds 
  
  cmd : IDENTIFIER ASSIGN expr              
      | CALL IDENTIFIER                     
      | READ LPAREN IDENTIFIER RPAREN       
      | PRINT LPAREN expr RPAREN            
      | IF expr THEN cmdseq ELSE cmdseq FI  
      | WHILE expr DO cmdseq OD             

  expr: expr LEQ expr   
        | expr LT expr 
        | expr EQ expr 
        | expr NEQ expr
        | expr GT expr 
        | expr GEQ expr
        
        | expr RPLUS expr            
        | expr RMINUS expr           
        | expr RTIMES expr           
        | expr RDIV expr             
        | INVERSE expr              
        | MAKERAT LPAREN expr COMMA expr RPAREN  
        | RATOF LPAREN expr RPAREN         

        | expr PLUS expr     
        | expr MINUS expr    
        | expr TIMES expr    
        | expr DIV expr      
        | expr MOD expr      
        
        | NOT expr        
        | expr AND expr     
        | expr OR expr      
        
        | LPAREN expr RPAREN 

        | NEG expr 
        
        | TRUE  
        | FALSE 
        
        | INTNUMERAL
        | RATNUMERAL 
        | IDENTIFIER 
```

Design Decisions : 
1. Procedures must have their unique identifiers
2. There is no UNARY '+' operator
3. The functions inverse, make_rat and rat take their input within parentheses. eg : rat(x) where x is an integer expression
4. Each of the print statements gives their output in a new line
5. Procedure definition must be declared before use in another procedure (or the global scope)
6. Following exceptions are thrown in case of errors : ProcedureNotDeclared, ProcedureRedeclaration, VariableNotDeclared, VariableRedeclaration, InvalidExpression, NotABooleanExpression, ExpressionTypeDoesNotMatchVariableType, NotBoolean, TypeMisMa, DivisionByZero. The occurrence of these exceptions is obvious by their names.

Running the program:
In the terminal go inside the directory and type
```
sml run.sml
```

In the interactive shell write
```
My.interpret(inputFile,outputFile)
```
where ```inputFile``` is the name of the input file written in the language Rational+PL0
and ```outputFile``` is the file where the output of the print statements is written

Files:
1. sources.cm - Is used to compile all the files
2. rational.sml - Contains the structures 'Rational' and 'BigInt'
3. rational.lex - Contains the lexer which generated the concrete parse tree
4. types.sml - Contains the structure 'DataTypes'
5. rational.grm - Contains the parser which is used to generate the Abstact Syntax Tree
6. ratparse.sml - Contains the structure 'My' which is used to compile the inputfile to an AST which is interpreted and writes the output in another file
7. run.sml - contains the commands to run the interpreter
8. Rest are auto-generated files

Acknowledgements:
1. ML-Lex Princetone Manual - https://www.cs.princeton.edu/~appel/modern/ml/ml-lex/manual.html
2. ML-Yacc Princeton Manual - https://www.cs.princeton.edu/~appel/modern/ml/ml-yacc/manual.html
3. User's guide to ML-Lex and ML-Yacc - https://citeseerx.ist.psu.edu/document?repid=rep1&type=pdf&doi=43247bea84122c52aa2d86aa86a8f4997825d419
4. The code for processing comments in the lexer has been taken from the interpreter of PASCAL language provided in the examples (downloaded with ml-yacc)
5. The structure 'DataTypes', the function 'My.compile' and some part of rational.grm is derived from https://github.com/ChinmayMittal/COL226/tree/main/Assignment%203 which has been modified suitably for the EBNF of Rational+PL0