structure DataTypes = 
struct
datatype AST = START of BLK
and BLK = BLK of DEC * (CMD list) 
and DEC = DEC of VARDEC * (PROCDEF list)
and VARDEC = Vardec of ((string list * Type) * (string list * Type) * (string list * Type))
and PROCDEF = Procdef of string * BLK
and Type = Bool | bigint | rational
and CMD = ASSIGN of string * Exp 
         | WHILE of Exp * (CMD list) 
         | ITE of Exp * (CMD list) * (CMD list) 
         | READ of string 
         | CALL of string 
         | PRINT of Exp
and Exp = NOT of Exp
         | AND of Exp * Exp
         | OR of Exp * Exp

         | RPLUS of Exp * Exp
         | RMINUS of Exp * Exp
         | RTIMES of Exp * Exp
         | RDIV of Exp * Exp 

         | INV of Exp
         | MAKE of Exp * Exp
         | RAT of Exp

         | PLUS of Exp * Exp
         | MINUS of Exp * Exp
         | TIMES of Exp * Exp
         | DIV of Exp * Exp 
         | MOD of Exp * Exp 

         | NEG of Exp
         | LT of Exp * Exp
         | LEQ of Exp * Exp
         | NEQ of Exp * Exp
         | EQ of Exp * Exp
         | GT of Exp * Exp
         | GEQ of Exp * Exp 

         | Identifier of string 
         | TT
         | FF 
         | Intnumeral of BigInt.bigint 
         | Ratnumeral of Rational.rational
end;
