functor MyLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : My_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
open DataTypes ;
fun x (x) = x


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\052\000\002\000\051\000\003\000\050\000\006\000\049\000\
\\036\000\048\000\037\000\047\000\038\000\046\000\039\000\045\000\
\\042\000\044\000\043\000\043\000\045\000\042\000\000\000\
\\001\000\002\000\016\000\000\000\
\\001\000\002\000\021\000\000\000\
\\001\000\002\000\039\000\000\000\
\\001\000\002\000\059\000\000\000\
\\001\000\004\000\014\000\000\000\
\\001\000\005\000\036\000\000\000\
\\001\000\006\000\038\000\000\000\
\\001\000\006\000\040\000\000\000\
\\001\000\006\000\080\000\000\000\
\\001\000\006\000\081\000\000\000\
\\001\000\007\000\086\000\021\000\076\000\022\000\075\000\023\000\074\000\
\\024\000\073\000\025\000\072\000\026\000\071\000\027\000\070\000\
\\028\000\069\000\029\000\068\000\030\000\067\000\031\000\066\000\
\\032\000\065\000\033\000\064\000\034\000\063\000\035\000\062\000\
\\040\000\061\000\041\000\060\000\000\000\
\\001\000\007\000\087\000\000\000\
\\001\000\007\000\108\000\021\000\076\000\022\000\075\000\023\000\074\000\
\\024\000\073\000\025\000\072\000\026\000\071\000\027\000\070\000\
\\028\000\069\000\029\000\068\000\030\000\067\000\031\000\066\000\
\\032\000\065\000\033\000\064\000\034\000\063\000\035\000\062\000\
\\040\000\061\000\041\000\060\000\000\000\
\\001\000\007\000\111\000\021\000\076\000\022\000\075\000\023\000\074\000\
\\024\000\073\000\025\000\072\000\026\000\071\000\027\000\070\000\
\\028\000\069\000\029\000\068\000\030\000\067\000\031\000\066\000\
\\032\000\065\000\033\000\064\000\034\000\063\000\035\000\062\000\
\\040\000\061\000\041\000\060\000\000\000\
\\001\000\007\000\116\000\021\000\076\000\022\000\075\000\023\000\074\000\
\\024\000\073\000\025\000\072\000\026\000\071\000\027\000\070\000\
\\028\000\069\000\029\000\068\000\030\000\067\000\031\000\066\000\
\\032\000\065\000\033\000\064\000\034\000\063\000\035\000\062\000\
\\040\000\061\000\041\000\060\000\000\000\
\\001\000\008\000\000\000\000\000\
\\001\000\010\000\084\000\021\000\076\000\022\000\075\000\023\000\074\000\
\\024\000\073\000\025\000\072\000\026\000\071\000\027\000\070\000\
\\028\000\069\000\029\000\068\000\030\000\067\000\031\000\066\000\
\\032\000\065\000\033\000\064\000\034\000\063\000\035\000\062\000\
\\040\000\061\000\041\000\060\000\000\000\
\\001\000\011\000\113\000\000\000\
\\001\000\012\000\117\000\000\000\
\\001\000\014\000\077\000\021\000\076\000\022\000\075\000\023\000\074\000\
\\024\000\073\000\025\000\072\000\026\000\071\000\027\000\070\000\
\\028\000\069\000\029\000\068\000\030\000\067\000\031\000\066\000\
\\032\000\065\000\033\000\064\000\034\000\063\000\035\000\062\000\
\\040\000\061\000\041\000\060\000\000\000\
\\001\000\015\000\110\000\000\000\
\\001\000\016\000\020\000\000\000\
\\001\000\016\000\030\000\000\000\
\\001\000\016\000\033\000\000\000\
\\001\000\016\000\037\000\000\000\
\\001\000\016\000\056\000\000\000\
\\001\000\017\000\112\000\021\000\076\000\022\000\075\000\023\000\074\000\
\\024\000\073\000\025\000\072\000\026\000\071\000\027\000\070\000\
\\028\000\069\000\029\000\068\000\030\000\067\000\031\000\066\000\
\\032\000\065\000\033\000\064\000\034\000\063\000\035\000\062\000\
\\040\000\061\000\041\000\060\000\000\000\
\\001\000\046\000\054\000\000\000\
\\119\000\000\000\
\\120\000\000\000\
\\121\000\000\000\
\\122\000\000\000\
\\123\000\020\000\007\000\000\000\
\\124\000\000\000\
\\125\000\018\000\009\000\000\000\
\\126\000\000\000\
\\127\000\019\000\018\000\000\000\
\\128\000\000\000\
\\129\000\017\000\031\000\000\000\
\\130\000\000\000\
\\131\000\050\000\012\000\000\000\
\\132\000\000\000\
\\133\000\000\000\
\\134\000\000\000\
\\135\000\002\000\029\000\009\000\028\000\013\000\027\000\047\000\026\000\
\\048\000\025\000\049\000\024\000\000\000\
\\136\000\000\000\
\\137\000\021\000\076\000\022\000\075\000\023\000\074\000\024\000\073\000\
\\025\000\072\000\026\000\071\000\027\000\070\000\028\000\069\000\
\\029\000\068\000\030\000\067\000\031\000\066\000\032\000\065\000\
\\033\000\064\000\034\000\063\000\035\000\062\000\040\000\061\000\
\\041\000\060\000\000\000\
\\138\000\000\000\
\\139\000\000\000\
\\140\000\000\000\
\\141\000\000\000\
\\142\000\000\000\
\\143\000\027\000\070\000\028\000\069\000\029\000\068\000\030\000\067\000\
\\031\000\066\000\032\000\065\000\033\000\064\000\034\000\063\000\
\\035\000\062\000\000\000\
\\144\000\027\000\070\000\028\000\069\000\029\000\068\000\030\000\067\000\
\\031\000\066\000\032\000\065\000\033\000\064\000\034\000\063\000\
\\035\000\062\000\000\000\
\\145\000\021\000\076\000\022\000\075\000\025\000\072\000\026\000\071\000\
\\027\000\070\000\028\000\069\000\029\000\068\000\030\000\067\000\
\\031\000\066\000\032\000\065\000\033\000\064\000\034\000\063\000\
\\035\000\062\000\000\000\
\\146\000\021\000\076\000\022\000\075\000\025\000\072\000\026\000\071\000\
\\027\000\070\000\028\000\069\000\029\000\068\000\030\000\067\000\
\\031\000\066\000\032\000\065\000\033\000\064\000\034\000\063\000\
\\035\000\062\000\000\000\
\\147\000\027\000\070\000\028\000\069\000\029\000\068\000\030\000\067\000\
\\031\000\066\000\032\000\065\000\033\000\064\000\034\000\063\000\
\\035\000\062\000\000\000\
\\148\000\027\000\070\000\028\000\069\000\029\000\068\000\030\000\067\000\
\\031\000\066\000\032\000\065\000\033\000\064\000\034\000\063\000\
\\035\000\062\000\000\000\
\\149\000\034\000\063\000\035\000\062\000\000\000\
\\150\000\034\000\063\000\035\000\062\000\000\000\
\\151\000\000\000\
\\152\000\000\000\
\\153\000\000\000\
\\154\000\000\000\
\\155\000\000\000\
\\156\000\029\000\068\000\030\000\067\000\031\000\066\000\032\000\065\000\
\\033\000\064\000\034\000\063\000\035\000\062\000\000\000\
\\157\000\029\000\068\000\030\000\067\000\031\000\066\000\032\000\065\000\
\\033\000\064\000\034\000\063\000\035\000\062\000\000\000\
\\158\000\032\000\065\000\033\000\064\000\034\000\063\000\035\000\062\000\000\000\
\\159\000\032\000\065\000\033\000\064\000\034\000\063\000\035\000\062\000\000\000\
\\160\000\032\000\065\000\033\000\064\000\034\000\063\000\035\000\062\000\000\000\
\\161\000\000\000\
\\162\000\021\000\076\000\022\000\075\000\023\000\074\000\024\000\073\000\
\\025\000\072\000\026\000\071\000\027\000\070\000\028\000\069\000\
\\029\000\068\000\030\000\067\000\031\000\066\000\032\000\065\000\
\\033\000\064\000\034\000\063\000\035\000\062\000\000\000\
\\163\000\021\000\076\000\022\000\075\000\023\000\074\000\024\000\073\000\
\\025\000\072\000\026\000\071\000\027\000\070\000\028\000\069\000\
\\029\000\068\000\030\000\067\000\031\000\066\000\032\000\065\000\
\\033\000\064\000\034\000\063\000\035\000\062\000\040\000\061\000\000\000\
\\164\000\000\000\
\\165\000\000\000\
\\166\000\000\000\
\\167\000\000\000\
\\168\000\000\000\
\\169\000\000\000\
\\170\000\000\000\
\"
val actionRowNumbers =
"\033\000\035\000\041\000\005\000\
\\029\000\001\000\037\000\001\000\
\\022\000\031\000\002\000\030\000\
\\045\000\023\000\039\000\032\000\
\\001\000\024\000\041\000\033\000\
\\006\000\025\000\007\000\003\000\
\\008\000\000\000\000\000\028\000\
\\034\000\001\000\026\000\036\000\
\\042\000\043\000\044\000\045\000\
\\000\000\048\000\004\000\020\000\
\\000\000\077\000\076\000\000\000\
\\009\000\010\000\000\000\000\000\
\\079\000\080\000\078\000\017\000\
\\000\000\040\000\038\000\046\000\
\\011\000\012\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\005\000\
\\075\000\071\000\000\000\000\000\
\\063\000\013\000\005\000\047\000\
\\050\000\049\000\073\000\072\000\
\\062\000\061\000\060\000\059\000\
\\070\000\069\000\068\000\067\000\
\\066\000\058\000\057\000\055\000\
\\056\000\053\000\054\000\021\000\
\\014\000\027\000\074\000\018\000\
\\052\000\065\000\000\000\005\000\
\\015\000\019\000\064\000\051\000\
\\016\000"
val gotoT =
"\
\\001\000\116\000\002\000\004\000\003\000\003\000\004\000\002\000\
\\006\000\001\000\000\000\
\\007\000\006\000\000\000\
\\009\000\009\000\010\000\008\000\000\000\
\\011\000\011\000\000\000\
\\000\000\
\\005\000\013\000\000\000\
\\008\000\015\000\000\000\
\\005\000\017\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\012\000\021\000\013\000\020\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\030\000\000\000\
\\000\000\
\\009\000\032\000\010\000\008\000\000\000\
\\002\000\033\000\003\000\003\000\004\000\002\000\006\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\014\000\039\000\000\000\
\\014\000\051\000\000\000\
\\000\000\
\\000\000\
\\005\000\053\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\012\000\021\000\013\000\055\000\000\000\
\\014\000\056\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\014\000\076\000\000\000\
\\000\000\
\\000\000\
\\014\000\077\000\000\000\
\\000\000\
\\000\000\
\\014\000\080\000\000\000\
\\014\000\081\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\014\000\083\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\014\000\086\000\000\000\
\\014\000\087\000\000\000\
\\014\000\088\000\000\000\
\\014\000\089\000\000\000\
\\014\000\090\000\000\000\
\\014\000\091\000\000\000\
\\014\000\092\000\000\000\
\\014\000\093\000\000\000\
\\014\000\094\000\000\000\
\\014\000\095\000\000\000\
\\014\000\096\000\000\000\
\\014\000\097\000\000\000\
\\014\000\098\000\000\000\
\\014\000\099\000\000\000\
\\014\000\100\000\000\000\
\\014\000\101\000\000\000\
\\014\000\102\000\000\000\
\\011\000\103\000\000\000\
\\000\000\
\\000\000\
\\014\000\104\000\000\000\
\\014\000\105\000\000\000\
\\000\000\
\\000\000\
\\011\000\107\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\014\000\112\000\000\000\
\\011\000\113\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 117
val numrules = 52
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | RATNUMERAL of unit ->  (Rational.rational)
 | IDENTIFIER of unit ->  (string)
 | INTNUMERAL of unit ->  (BigInt.bigint)
 | expr of unit ->  (Type*Exp) | cmds of unit ->  (CMD list)
 | cmd of unit ->  (CMD) | cmdseq of unit ->  (CMD list)
 | procdef of unit ->  (PROCDEF) | procdec of unit ->  (PROCDEF list)
 | boolvardec of unit ->  ( ( string list ) *Type)
 | intvardec of unit ->  ( ( string list ) *Type)
 | ratvardec of unit ->  ( ( string list ) *Type)
 | varlist of unit ->  (string list) | vardec of unit ->  (VARDEC)
 | dec of unit ->  (DEC) | blk of unit ->  (BLK)
 | start of unit ->  (AST)
end
type svalue = MlyValue.svalue
type result = AST
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 7) => true | _ => false
val showTerminal =
fn (T 0) => "INTNUMERAL"
  | (T 1) => "IDENTIFIER"
  | (T 2) => "RATNUMERAL"
  | (T 3) => "LBRACE"
  | (T 4) => "RBRACE"
  | (T 5) => "LPAREN"
  | (T 6) => "RPAREN"
  | (T 7) => "EOF"
  | (T 8) => "IF"
  | (T 9) => "THEN"
  | (T 10) => "ELSE"
  | (T 11) => "FI"
  | (T 12) => "WHILE"
  | (T 13) => "DO"
  | (T 14) => "OD"
  | (T 15) => "SEMI"
  | (T 16) => "COMMA"
  | (T 17) => "INT"
  | (T 18) => "BOOL"
  | (T 19) => "RAT"
  | (T 20) => "LT"
  | (T 21) => "LEQ"
  | (T 22) => "NEQ"
  | (T 23) => "EQ"
  | (T 24) => "GT"
  | (T 25) => "GEQ"
  | (T 26) => "PLUS"
  | (T 27) => "MINUS"
  | (T 28) => "TIMES"
  | (T 29) => "DIV"
  | (T 30) => "MOD"
  | (T 31) => "RPLUS"
  | (T 32) => "RMINUS"
  | (T 33) => "RTIMES"
  | (T 34) => "RDIV"
  | (T 35) => "INVERSE"
  | (T 36) => "MAKERAT"
  | (T 37) => "RATOF"
  | (T 38) => "NOT"
  | (T 39) => "AND"
  | (T 40) => "OR"
  | (T 41) => "TRUE"
  | (T 42) => "FALSE"
  | (T 43) => "VAR"
  | (T 44) => "NEG"
  | (T 45) => "ASSIGN"
  | (T 46) => "READ"
  | (T 47) => "CALL"
  | (T 48) => "PRINT"
  | (T 49) => "PROC"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 49) $$ (T 48) $$ (T 47) $$ (T 46) $$ (T 45) $$ (T 44) $$ (T 43)
 $$ (T 42) $$ (T 41) $$ (T 40) $$ (T 39) $$ (T 38) $$ (T 37) $$ (T 36)
 $$ (T 35) $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31) $$ (T 30) $$ (T 29)
 $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24) $$ (T 23) $$ (T 22)
 $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15)
 $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8)
 $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.blk blk1, blk1left, blk1right)) :: rest671)
) => let val  result = MlyValue.start (fn _ => let val  (blk as blk1)
 = blk1 ()
 in (START(blk))
end)
 in ( LrTable.NT 0, ( result, blk1left, blk1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.cmdseq cmdseq1, _, cmdseq1right)) :: ( _, ( 
MlyValue.dec dec1, dec1left, _)) :: rest671)) => let val  result = 
MlyValue.blk (fn _ => let val  (dec as dec1) = dec1 ()
 val  (cmdseq as cmdseq1) = cmdseq1 ()
 in (BLK(dec,cmdseq))
end)
 in ( LrTable.NT 1, ( result, dec1left, cmdseq1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.procdec procdec1, _, procdec1right)) :: ( _,
 ( MlyValue.vardec vardec1, vardec1left, _)) :: rest671)) => let val  
result = MlyValue.dec (fn _ => let val  (vardec as vardec1) = vardec1
 ()
 val  (procdec as procdec1) = procdec1 ()
 in (DEC(vardec,procdec))
end)
 in ( LrTable.NT 2, ( result, vardec1left, procdec1right), rest671)

end
|  ( 3, ( ( _, ( MlyValue.boolvardec boolvardec1, _, boolvardec1right)
) :: ( _, ( MlyValue.intvardec intvardec1, _, _)) :: ( _, ( 
MlyValue.ratvardec ratvardec1, ratvardec1left, _)) :: rest671)) => let
 val  result = MlyValue.vardec (fn _ => let val  (ratvardec as 
ratvardec1) = ratvardec1 ()
 val  (intvardec as intvardec1) = intvardec1 ()
 val  (boolvardec as boolvardec1) = boolvardec1 ()
 in (Vardec(ratvardec,intvardec,boolvardec))
end)
 in ( LrTable.NT 3, ( result, ratvardec1left, boolvardec1right), 
rest671)
end
|  ( 4, ( rest671)) => let val  result = MlyValue.ratvardec (fn _ => (
[],rational))
 in ( LrTable.NT 5, ( result, defaultPos, defaultPos), rest671)
end
|  ( 5, ( ( _, ( _, _, SEMI1right)) :: ( _, ( MlyValue.varlist 
varlist1, _, _)) :: ( _, ( _, RAT1left, _)) :: rest671)) => let val  
result = MlyValue.ratvardec (fn _ => let val  (varlist as varlist1) = 
varlist1 ()
 in ((varlist,rational))
end)
 in ( LrTable.NT 5, ( result, RAT1left, SEMI1right), rest671)
end
|  ( 6, ( rest671)) => let val  result = MlyValue.intvardec (fn _ => (
[],bigint))
 in ( LrTable.NT 6, ( result, defaultPos, defaultPos), rest671)
end
|  ( 7, ( ( _, ( _, _, SEMI1right)) :: ( _, ( MlyValue.varlist 
varlist1, _, _)) :: ( _, ( _, INT1left, _)) :: rest671)) => let val  
result = MlyValue.intvardec (fn _ => let val  (varlist as varlist1) = 
varlist1 ()
 in ((varlist,bigint))
end)
 in ( LrTable.NT 6, ( result, INT1left, SEMI1right), rest671)
end
|  ( 8, ( rest671)) => let val  result = MlyValue.boolvardec (fn _ =>
 ([],Bool))
 in ( LrTable.NT 7, ( result, defaultPos, defaultPos), rest671)
end
|  ( 9, ( ( _, ( _, _, SEMI1right)) :: ( _, ( MlyValue.varlist 
varlist1, _, _)) :: ( _, ( _, BOOL1left, _)) :: rest671)) => let val  
result = MlyValue.boolvardec (fn _ => let val  (varlist as varlist1) =
 varlist1 ()
 in ((varlist,Bool))
end)
 in ( LrTable.NT 7, ( result, BOOL1left, SEMI1right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.IDENTIFIER IDENTIFIER1, IDENTIFIER1left, 
IDENTIFIER1right)) :: rest671)) => let val  result = MlyValue.varlist
 (fn _ => let val  (IDENTIFIER as IDENTIFIER1) = IDENTIFIER1 ()
 in (([IDENTIFIER]))
end)
 in ( LrTable.NT 4, ( result, IDENTIFIER1left, IDENTIFIER1right), 
rest671)
end
|  ( 11, ( ( _, ( MlyValue.varlist varlist1, _, varlist1right)) :: _
 :: ( _, ( MlyValue.IDENTIFIER IDENTIFIER1, IDENTIFIER1left, _)) :: 
rest671)) => let val  result = MlyValue.varlist (fn _ => let val  (
IDENTIFIER as IDENTIFIER1) = IDENTIFIER1 ()
 val  (varlist as varlist1) = varlist1 ()
 in ((IDENTIFIER::varlist))
end)
 in ( LrTable.NT 4, ( result, IDENTIFIER1left, varlist1right), rest671
)
end
|  ( 12, ( rest671)) => let val  result = MlyValue.procdec (fn _ => (
([])))
 in ( LrTable.NT 8, ( result, defaultPos, defaultPos), rest671)
end
|  ( 13, ( ( _, ( MlyValue.procdec procdec1, _, procdec1right)) :: _
 :: ( _, ( MlyValue.procdef procdef1, procdef1left, _)) :: rest671))
 => let val  result = MlyValue.procdec (fn _ => let val  (procdef as 
procdef1) = procdef1 ()
 val  (procdec as procdec1) = procdec1 ()
 in (procdef::procdec)
end)
 in ( LrTable.NT 8, ( result, procdef1left, procdec1right), rest671)

end
|  ( 14, ( ( _, ( MlyValue.blk blk1, _, blk1right)) :: ( _, ( 
MlyValue.IDENTIFIER IDENTIFIER1, _, _)) :: ( _, ( _, PROC1left, _)) ::
 rest671)) => let val  result = MlyValue.procdef (fn _ => let val  (
IDENTIFIER as IDENTIFIER1) = IDENTIFIER1 ()
 val  (blk as blk1) = blk1 ()
 in (Procdef(IDENTIFIER,blk))
end)
 in ( LrTable.NT 9, ( result, PROC1left, blk1right), rest671)
end
|  ( 15, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.cmds cmds1,
 _, _)) :: ( _, ( _, LBRACE1left, _)) :: rest671)) => let val  result
 = MlyValue.cmdseq (fn _ => let val  (cmds as cmds1) = cmds1 ()
 in ((cmds))
end)
 in ( LrTable.NT 10, ( result, LBRACE1left, RBRACE1right), rest671)

end
|  ( 16, ( rest671)) => let val  result = MlyValue.cmds (fn _ => (([])
))
 in ( LrTable.NT 12, ( result, defaultPos, defaultPos), rest671)
end
|  ( 17, ( ( _, ( MlyValue.cmds cmds1, _, cmds1right)) :: _ :: ( _, ( 
MlyValue.cmd cmd1, cmd1left, _)) :: rest671)) => let val  result = 
MlyValue.cmds (fn _ => let val  (cmd as cmd1) = cmd1 ()
 val  (cmds as cmds1) = cmds1 ()
 in ((cmd::cmds))
end)
 in ( LrTable.NT 12, ( result, cmd1left, cmds1right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.expr expr1, _, expr1right)) :: _ :: ( _, ( 
MlyValue.IDENTIFIER IDENTIFIER1, IDENTIFIER1left, _)) :: rest671)) =>
 let val  result = MlyValue.cmd (fn _ => let val  (IDENTIFIER as 
IDENTIFIER1) = IDENTIFIER1 ()
 val  (expr as expr1) = expr1 ()
 in (ASSIGN(IDENTIFIER,#2 expr))
end)
 in ( LrTable.NT 11, ( result, IDENTIFIER1left, expr1right), rest671)

end
|  ( 19, ( ( _, ( MlyValue.IDENTIFIER IDENTIFIER1, _, IDENTIFIER1right
)) :: ( _, ( _, CALL1left, _)) :: rest671)) => let val  result = 
MlyValue.cmd (fn _ => let val  (IDENTIFIER as IDENTIFIER1) = 
IDENTIFIER1 ()
 in (CALL(IDENTIFIER))
end)
 in ( LrTable.NT 11, ( result, CALL1left, IDENTIFIER1right), rest671)

end
|  ( 20, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.IDENTIFIER 
IDENTIFIER1, _, _)) :: _ :: ( _, ( _, READ1left, _)) :: rest671)) =>
 let val  result = MlyValue.cmd (fn _ => let val  (IDENTIFIER as 
IDENTIFIER1) = IDENTIFIER1 ()
 in (READ(IDENTIFIER))
end)
 in ( LrTable.NT 11, ( result, READ1left, RPAREN1right), rest671)
end
|  ( 21, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.expr expr1,
 _, _)) :: _ :: ( _, ( _, PRINT1left, _)) :: rest671)) => let val  
result = MlyValue.cmd (fn _ => let val  (expr as expr1) = expr1 ()
 in (PRINT(#2 expr))
end)
 in ( LrTable.NT 11, ( result, PRINT1left, RPAREN1right), rest671)
end
|  ( 22, ( ( _, ( _, _, FI1right)) :: ( _, ( MlyValue.cmdseq cmdseq2,
 _, _)) :: _ :: ( _, ( MlyValue.cmdseq cmdseq1, _, _)) :: _ :: ( _, ( 
MlyValue.expr expr1, _, _)) :: ( _, ( _, IF1left, _)) :: rest671)) =>
 let val  result = MlyValue.cmd (fn _ => let val  (expr as expr1) = 
expr1 ()
 val  cmdseq1 = cmdseq1 ()
 val  cmdseq2 = cmdseq2 ()
 in (ITE(#2 expr,cmdseq1,cmdseq2))
end)
 in ( LrTable.NT 11, ( result, IF1left, FI1right), rest671)
end
|  ( 23, ( ( _, ( _, _, OD1right)) :: ( _, ( MlyValue.cmdseq cmdseq1,
 _, _)) :: _ :: ( _, ( MlyValue.expr expr1, _, _)) :: ( _, ( _, 
WHILE1left, _)) :: rest671)) => let val  result = MlyValue.cmd (fn _
 => let val  (expr as expr1) = expr1 ()
 val  (cmdseq as cmdseq1) = cmdseq1 ()
 in (WHILE(#2 expr,cmdseq))
end)
 in ( LrTable.NT 11, ( result, WHILE1left, OD1right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (Bool , LEQ(#2 expr1 , #2 expr2))
end)
 in ( LrTable.NT 13, ( result, expr1left, expr2right), rest671)
end
|  ( 25, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (Bool , LT(#2 expr1 , #2 expr2))
end)
 in ( LrTable.NT 13, ( result, expr1left, expr2right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (Bool , EQ(#2 expr1 , #2 expr2))
end)
 in ( LrTable.NT 13, ( result, expr1left, expr2right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (Bool , NEQ(#2 expr1 , #2 expr2))
end)
 in ( LrTable.NT 13, ( result, expr1left, expr2right), rest671)
end
|  ( 28, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (Bool , GT(#2 expr1 , #2 expr2))
end)
 in ( LrTable.NT 13, ( result, expr1left, expr2right), rest671)
end
|  ( 29, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in (Bool , GEQ(#2 expr1 , #2 expr2))
end)
 in ( LrTable.NT 13, ( result, expr1left, expr2right), rest671)
end
|  ( 30, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in ((rational , RPLUS(#2 expr1 , #2 expr2)))
end)
 in ( LrTable.NT 13, ( result, expr1left, expr2right), rest671)
end
|  ( 31, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in ((rational , RMINUS(#2 expr1 , #2 expr2)))
end)
 in ( LrTable.NT 13, ( result, expr1left, expr2right), rest671)
end
|  ( 32, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in ((rational , RTIMES(#2 expr1 , #2 expr2)))
end)
 in ( LrTable.NT 13, ( result, expr1left, expr2right), rest671)
end
|  ( 33, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in ((rational , RDIV(#2 expr1 , #2 expr2)))
end)
 in ( LrTable.NT 13, ( result, expr1left, expr2right), rest671)
end
|  ( 34, ( ( _, ( MlyValue.expr expr1, _, expr1right)) :: ( _, ( _, 
INVERSE1left, _)) :: rest671)) => let val  result = MlyValue.expr (fn
 _ => let val  (expr as expr1) = expr1 ()
 in ((rational , INV(#2 expr)))
end)
 in ( LrTable.NT 13, ( result, INVERSE1left, expr1right), rest671)
end
|  ( 35, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.expr expr2,
 _, _)) :: _ :: ( _, ( MlyValue.expr expr1, _, _)) :: _ :: ( _, ( _, 
MAKERAT1left, _)) :: rest671)) => let val  result = MlyValue.expr (fn
 _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in ((rational , MAKE(#2 expr1 , #2 expr2)))
end)
 in ( LrTable.NT 13, ( result, MAKERAT1left, RPAREN1right), rest671)

end
|  ( 36, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.expr expr1,
 _, _)) :: _ :: ( _, ( _, RATOF1left, _)) :: rest671)) => let val  
result = MlyValue.expr (fn _ => let val  (expr as expr1) = expr1 ()
 in ((rational , RAT(#2 expr)))
end)
 in ( LrTable.NT 13, ( result, RATOF1left, RPAREN1right), rest671)
end
|  ( 37, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in ((bigint , PLUS(#2 expr1 , #2 expr2)))
end)
 in ( LrTable.NT 13, ( result, expr1left, expr2right), rest671)
end
|  ( 38, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in ((bigint , MINUS(#2 expr1 , #2 expr2)))
end)
 in ( LrTable.NT 13, ( result, expr1left, expr2right), rest671)
end
|  ( 39, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in ((bigint , TIMES(#2 expr1 , #2 expr2)))
end)
 in ( LrTable.NT 13, ( result, expr1left, expr2right), rest671)
end
|  ( 40, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in ((bigint , DIV(#2 expr1 , #2 expr2)))
end)
 in ( LrTable.NT 13, ( result, expr1left, expr2right), rest671)
end
|  ( 41, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in ((bigint , MOD(#2 expr1 , #2 expr2)))
end)
 in ( LrTable.NT 13, ( result, expr1left, expr2right), rest671)
end
|  ( 42, ( ( _, ( MlyValue.expr expr1, _, expr1right)) :: ( _, ( _, 
NOT1left, _)) :: rest671)) => let val  result = MlyValue.expr (fn _ =>
 let val  (expr as expr1) = expr1 ()
 in ((Bool , NOT(#2 expr)))
end)
 in ( LrTable.NT 13, ( result, NOT1left, expr1right), rest671)
end
|  ( 43, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in ((Bool , AND(#2 expr1 , #2 expr2)))
end)
 in ( LrTable.NT 13, ( result, expr1left, expr2right), rest671)
end
|  ( 44, ( ( _, ( MlyValue.expr expr2, _, expr2right)) :: _ :: ( _, ( 
MlyValue.expr expr1, expr1left, _)) :: rest671)) => let val  result = 
MlyValue.expr (fn _ => let val  expr1 = expr1 ()
 val  expr2 = expr2 ()
 in ((Bool , OR(#2 expr1 , #2 expr2)))
end)
 in ( LrTable.NT 13, ( result, expr1left, expr2right), rest671)
end
|  ( 45, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.expr expr1,
 _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result
 = MlyValue.expr (fn _ => let val  (expr as expr1) = expr1 ()
 in ((expr))
end)
 in ( LrTable.NT 13, ( result, LPAREN1left, RPAREN1right), rest671)

end
|  ( 46, ( ( _, ( MlyValue.expr expr1, _, expr1right)) :: ( _, ( _, 
NEG1left, _)) :: rest671)) => let val  result = MlyValue.expr (fn _ =>
 let val  (expr as expr1) = expr1 ()
 in ((#1 expr,NEG(#2 expr)))
end)
 in ( LrTable.NT 13, ( result, NEG1left, expr1right), rest671)
end
|  ( 47, ( ( _, ( _, TRUE1left, TRUE1right)) :: rest671)) => let val  
result = MlyValue.expr (fn _ => ((Bool,TT)))
 in ( LrTable.NT 13, ( result, TRUE1left, TRUE1right), rest671)
end
|  ( 48, ( ( _, ( _, FALSE1left, FALSE1right)) :: rest671)) => let
 val  result = MlyValue.expr (fn _ => ((Bool,FF)))
 in ( LrTable.NT 13, ( result, FALSE1left, FALSE1right), rest671)
end
|  ( 49, ( ( _, ( MlyValue.INTNUMERAL INTNUMERAL1, INTNUMERAL1left, 
INTNUMERAL1right)) :: rest671)) => let val  result = MlyValue.expr (fn
 _ => let val  (INTNUMERAL as INTNUMERAL1) = INTNUMERAL1 ()
 in ((bigint,Intnumeral(INTNUMERAL)))
end)
 in ( LrTable.NT 13, ( result, INTNUMERAL1left, INTNUMERAL1right), 
rest671)
end
|  ( 50, ( ( _, ( MlyValue.RATNUMERAL RATNUMERAL1, RATNUMERAL1left, 
RATNUMERAL1right)) :: rest671)) => let val  result = MlyValue.expr (fn
 _ => let val  (RATNUMERAL as RATNUMERAL1) = RATNUMERAL1 ()
 in ((rational,Ratnumeral(RATNUMERAL)))
end)
 in ( LrTable.NT 13, ( result, RATNUMERAL1left, RATNUMERAL1right), 
rest671)
end
|  ( 51, ( ( _, ( MlyValue.IDENTIFIER IDENTIFIER1, IDENTIFIER1left, 
IDENTIFIER1right)) :: rest671)) => let val  result = MlyValue.expr (fn
 _ => let val  (IDENTIFIER as IDENTIFIER1) = IDENTIFIER1 ()
 in ((Bool,Identifier(IDENTIFIER)))
end)
 in ( LrTable.NT 13, ( result, IDENTIFIER1left, IDENTIFIER1right), 
rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.start x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : My_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun INTNUMERAL (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.INTNUMERAL (fn () => i),p1,p2))
fun IDENTIFIER (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.IDENTIFIER (fn () => i),p1,p2))
fun RATNUMERAL (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.RATNUMERAL (fn () => i),p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun FI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun OD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMI (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun INT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun BOOL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun RAT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun LEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun NEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun GEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun DIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun MOD (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun RPLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun RMINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun RTIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun RDIV (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun INVERSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun MAKERAT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun RATOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
fun TRUE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
fun FALSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 42,(
ParserData.MlyValue.VOID,p1,p2))
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 43,(
ParserData.MlyValue.VOID,p1,p2))
fun NEG (p1,p2) = Token.TOKEN (ParserData.LrTable.T 44,(
ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 45,(
ParserData.MlyValue.VOID,p1,p2))
fun READ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 46,(
ParserData.MlyValue.VOID,p1,p2))
fun CALL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 47,(
ParserData.MlyValue.VOID,p1,p2))
fun PRINT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 48,(
ParserData.MlyValue.VOID,p1,p2))
fun PROC (p1,p2) = Token.TOKEN (ParserData.LrTable.T 49,(
ParserData.MlyValue.VOID,p1,p2))
end
end
