structure Tokens = Tokens

type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult= (svalue,pos) token

val pos = ref 0
fun eof () = Tokens.EOF(!pos,!pos)
fun error (e,l : int,_) = TextIO.output (TextIO.stdOut, String.concat[
    "line ", (Int.toString l), ": ", e, "\n"
      ])

%%
%header (functor MyLexFun(structure Tokens: My_TOKENS));
%state COMMENT R;
alpha=[A-Za-z_];
digit=[0-9];
ws=[\ \t];
all=[.];
%%
<INITIAL>\n       => ( pos:= (!pos) + 1; lex()); 
<INITIAL>{ws}+    => (lex());

<INITIAL>"integer"       => (Tokens.INT(!pos , !pos )) ;
<INITIAL>"boolean"       => (Tokens.BOOL(!pos , !pos )) ;
<INITIAL>"rational"    => (Tokens.RAT(!pos, !pos)) ;
<INITIAL>"tt" => ( Tokens.TRUE(!pos , !pos ) ) ;
<INITIAL>"ff" => (Tokens.FALSE(!pos , !pos )) ;  
<INITIAL>"if"       => (Tokens.IF(!pos , !pos )) ;
<INITIAL>"then"      => (Tokens.THEN(!pos, !pos)) ;
<INITIAL>"else" => (Tokens.ELSE(!pos , !pos )) ; 
<INITIAL>"fi"       => (Tokens.FI(!pos , !pos )) ;
<INITIAL>"while"       => (Tokens.WHILE(!pos , !pos )) ;
<INITIAL>"do"       => (Tokens.DO(!pos , !pos )) ;
<INITIAL>"od"       => (Tokens.OD(!pos , !pos )) ;
<INITIAL>"procedure"       => (Tokens.PROC(!pos , !pos )) ;
<INITIAL>"print"       => (Tokens.PRINT(!pos , !pos )) ;
<INITIAL>"read"       => (Tokens.READ(!pos , !pos )) ;
<INITIAL>"call"       => (Tokens.CALL(!pos , !pos )) ;

<INITIAL>"~"       => (Tokens.NEG(!pos , !pos)) ; 

<INITIAL>".+."       => (Tokens.RPLUS(!pos , !pos )) ;
<INITIAL>".-."       => (Tokens.RMINUS(!pos , !pos )) ;
<INITIAL>".*."       => (Tokens.RTIMES(!pos , !pos )) ;
<INITIAL>"./."       => (Tokens.RDIV(!pos , !pos )) ;
<INITIAL>"inverse"   => (Tokens.INVERSE(!pos, !pos)) ;
<INITIAL>"make_rat"  => (Tokens.MAKERAT(!pos, !pos)) ;
<INITIAL>"rat"       => (Tokens.RATOF(!pos, !pos)) ;
<INITIAL>"fromDecimal" => (YYBEGIN R;lex());
<R>\n                 => ( pos:= (!pos) + 1; lex()); 
<R>{ws}+              => (lex());
<R>"("                => (lex());
<R>("~")?{digit}*("."{digit}*"("{digit}+")") => (Tokens.RATNUMERAL (Rational.fromDecimal (yytext), !pos, !pos));
<R>")"                => (YYBEGIN INITIAL;lex());

<INITIAL>"+"       => (Tokens.PLUS(!pos , !pos )) ;
<INITIAL>"-"       => (Tokens.MINUS(!pos , !pos )) ;
<INITIAL>"*"       => (Tokens.TIMES(!pos , !pos )) ;
<INITIAL>"/"       => (Tokens.DIV(!pos , !pos )) ;
<INITIAL>"%"       => (Tokens.MOD(!pos , !pos )) ;

<INITIAL>"!"      => (Tokens.NOT(!pos , !pos )) ; 
<INITIAL>"&&"       => ( Tokens.AND(!pos , !pos )) ; 
<INITIAL>"||"       => (Tokens.OR(!pos , !pos )) ;

<INITIAL>"<="       => (Tokens.LEQ(!pos , !pos )) ;
<INITIAL>">="       => (Tokens.GEQ(!pos , !pos )) ;
<INITIAL>"<>"       => (Tokens.NEQ(!pos , !pos )) ;
<INITIAL>">"       => (Tokens.GT(!pos , !pos )) ;
<INITIAL>"="       => (Tokens.EQ(!pos , !pos )) ;
<INITIAL>"<"       => (Tokens.LT(!pos , !pos )) ;

<INITIAL>":="       => (Tokens.ASSIGN(!pos , !pos )) ;

<INITIAL>"("      => (Tokens.LPAREN(!pos , !pos ));
<INITIAL>")"      => (Tokens.RPAREN(!pos , !pos )) ; 
<INITIAL>"}"      => (Tokens.RBRACE(!pos , !pos )) ; 
<INITIAL>"{"      => (Tokens.LBRACE(!pos , !pos )) ; 

<INITIAL>";"       => (Tokens.SEMI(!pos , !pos )) ;
<INITIAL>","       => (Tokens.COMMA(!pos , !pos )) ;

<INITIAL>{alpha}({alpha} | {digit})* => ( Tokens.IDENTIFIER( yytext ,!pos , !pos )) ; 

<INITIAL>("~")?{digit}*("."{digit}*"("{digit}+")") => (Tokens.RATNUMERAL (Rational.fromDecimal (yytext), !pos, !pos));
<INITIAL>("~")?{digit}+ => ( Tokens.INTNUMERAL( BigInt.fromString(yytext) , !pos , !pos ) ) ; 
<INITIAL>"(*" => (YYBEGIN COMMENT; lex());
<COMMENT>\n+		=> (pos := (!pos) + (String.size yytext); lex());
<COMMENT>[^()*\n]+	=> (lex());
<COMMENT>"(*"		=> (lex());
<COMMENT>"*)"		=> (YYBEGIN INITIAL; lex());
<COMMENT>[*()]	=> (lex());