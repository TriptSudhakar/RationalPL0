structure MyLrVals = MyLrValsFun(
structure Token = LrParser.Token);
structure MyLex = MyLexFun(
structure Tokens = MyLrVals.Tokens);
structure MyParser = Join(
structure ParserData = MyLrVals.ParserData
structure Lex=MyLex
structure LrParser=LrParser);

structure My :
sig val compile : string -> DataTypes.AST
    val interpret : string * string -> unit
end =
    struct
    exception MyError;
    fun compile (fileName) =
    let
        val inStream = TextIO.openIn fileName;
        val grab : int -> string = fn
        n => if TextIO.endOfStream inStream
            then ""
            else TextIO.inputN (inStream,n);
        val printError : string * int * int -> unit = fn
        (msg,line,col) =>
        print (fileName^"["^Int.toString line^":"
        ^Int.toString col^"] "^msg^"\n");
        val (tree,rem) = MyParser.parse
        (15,
        (MyParser.makeLexer grab ),
        printError,
        ()) 
        handle MyParser.ParseError => raise MyError ; 
        (* Close the source program file *)
        val _ = TextIO.closeIn inStream;

    in 
        tree 
    end

    exception ProcedureNotDeclared;
    exception ProcedureRedeclaration;
    exception VariableNotDeclared;
    exception VariableRedeclaration;
    exception InvalidExpression;
    exception NotABooleanExpression;
    exception ExpressionTypeDoesNotMatchVariableType;
    exception NotBoolean;
    exception TypeMisMatchException;
    exception DivisionByZero;
    fun interpret(inputFile,outputFile) = 
    let val typetable : (int,(string, DataTypes.Type) HashTable.hash_table) HashTable.hash_table = HashTable.mkTable (Word.fromInt, op=) (42, Fail "Type nhi mila\n");
        val rattable : (int,(string, Rational.rational) HashTable.hash_table) HashTable.hash_table = HashTable.mkTable (Word.fromInt, op=) (42, Fail "rational nhi mil rha\n");
        val inttable : (int,(string, BigInt.bigint) HashTable.hash_table) HashTable.hash_table = HashTable.mkTable (Word.fromInt, op=) (42, Fail "integer nhi mil rha bhai\n");
        val booltable : (int,(string, bool) HashTable.hash_table) HashTable.hash_table = HashTable.mkTable (Word.fromInt, op=) (42, Fail "Bool nhi mil rha yaar\n");
        val parent : (int,int) HashTable.hash_table = HashTable.mkTable (Word.fromInt, op=) (42, Fail "Anaarth hu mai\n");
        val procedure: (string,int) HashTable.hash_table = HashTable.mkTable (HashString.hashString, op=) (42, Fail "function to declare karle pehle\n");
        val cmdlist : (int,DataTypes.CMD list) HashTable.hash_table = HashTable.mkTable (Word.fromInt, op=) (42, Fail "command to likhle kuch pehle\n");

        val scope = ref 0;
        fun increment (x : int ref) = x := !x + 1;

        fun getInput( message : string ) =
        ( print ( message ^ "\n") ; 
            let
            val str= valOf(TextIO.inputLine TextIO.stdIn)
            in
            String.substring ( str , 0 ,  (String.size str) -1) 
            end)

        fun inserttype([],t,table) = ()
        | inserttype(x::xs,t,table) = if isSome(HashTable.find table x) then raise VariableRedeclaration else (HashTable.insert table (x,t); inserttype(xs,t,table))

        fun find_var(x,s) = if isSome(HashTable.find typetable s) andalso isSome(HashTable.find (HashTable.lookup typetable s) x) then true 
                            else if s = 0 then false else find_var(x,(HashTable.lookup parent s))

        fun findtype(x,s) = if isSome(HashTable.find typetable s) andalso isSome(HashTable.find (HashTable.lookup typetable s) x) 
                            then HashTable.find (HashTable.lookup typetable s) x
                            else if s = 0 then NONE else findtype(x,(HashTable.lookup parent s))

        fun findscope(x,s) =if isSome(HashTable.find typetable s) andalso isSome(HashTable.find (HashTable.lookup typetable s) x) 
                            then SOME s else if s = 0 then NONE else findscope(x,(HashTable.lookup parent s))

        fun check_rat(term,curr) = 
        case term of DataTypes.RPLUS(x,y) => check_rat(x,curr) andalso check_rat(y,curr)
                    | DataTypes.RMINUS(x,y) => check_rat(x,curr) andalso check_rat(y,curr)
                    | DataTypes.RTIMES(x,y) => check_rat(x,curr) andalso check_rat(y,curr)
                    | DataTypes.RDIV(x,y) => check_rat(x,curr) andalso check_rat(y,curr)
                    | DataTypes.INV(x) => check_rat(x,curr)
                    | DataTypes.MAKE(x,y) => check_int(x,curr) andalso check_int(y,curr)
                    | DataTypes.RAT(x) => check_int(x,curr)
                    | DataTypes.NEG(x) => check_rat(x,curr)
                    | DataTypes.Ratnumeral(x) => true
                    | DataTypes.Identifier(id) => 
                    let val t = findtype(id,curr)
                    in if isSome(t) andalso valOf(t) = DataTypes.rational then true else false
                    end
                    | _ => false

        and check_int(term,curr) =
        case term of DataTypes.PLUS(x,y) => check_int(x,curr) andalso check_int(y,curr)
                    | DataTypes.MINUS(x,y) => check_int(x,curr) andalso check_int(y,curr)
                    | DataTypes.TIMES(x,y) => check_int(x,curr) andalso check_int(y,curr)
                    | DataTypes.DIV(x,y) => check_int(x,curr) andalso check_int(y,curr)
                    | DataTypes.MOD(x,y) => check_int(x,curr) andalso check_int(y,curr)
                    | DataTypes.NEG(x) => check_int(x,curr)
                    | DataTypes.Intnumeral(x) => true
                    | DataTypes.Identifier(id) =>
                    let val t = findtype(id,curr)
                    in if isSome(t) andalso valOf(t) = DataTypes.bigint then true else false
                    end
                    | _ => false

        and check_bool(term,curr) = 
        case term of DataTypes.NOT(x) => check_bool(x,curr)
                    | DataTypes.AND(x,y) => check_bool(x,curr) andalso check_bool(y,curr)
                    | DataTypes.OR(x,y) => check_bool(x,curr) andalso check_bool(y,curr)

                    | DataTypes.GEQ(x,y) => (check_int(x,curr) andalso check_int(y,curr)) orelse (check_rat(x,curr) andalso check_rat(y,curr))
                    | DataTypes.LEQ(x,y) => (check_int(x,curr) andalso check_int(y,curr)) orelse (check_rat(x,curr) andalso check_rat(y,curr))
                    | DataTypes.LT(x,y) => (check_int(x,curr) andalso check_int(y,curr)) orelse (check_rat(x,curr) andalso check_rat(y,curr))
                    | DataTypes.GT(x,y) => (check_int(x,curr) andalso check_int(y,curr)) orelse (check_rat(x,curr) andalso check_rat(y,curr))

                    | DataTypes.EQ(x,y) => (check_int(x,curr) andalso check_int(y,curr)) orelse (check_rat(x,curr) andalso check_rat(y,curr)) orelse (check_bool(x,curr) andalso check_bool(y,curr))
                    | DataTypes.NEQ(x,y) => (check_int(x,curr) andalso check_int(y,curr)) orelse (check_rat(x,curr) andalso check_rat(y,curr)) orelse (check_bool(x,curr) andalso check_bool(y,curr))
                    | DataTypes.TT => true
                    | DataTypes.FF => true
                    | DataTypes.Identifier(id) =>
                    let val t = findtype(id,curr)
                    in if isSome(t) andalso valOf(t) = DataTypes.Bool then true else false
                    end
                    | _ => false          

        fun valid_expr(term,curr) = 
        case term of DataTypes.NOT(x) => check_bool(x,curr)
                    | DataTypes.AND(x,y) => check_bool(x,curr) andalso check_bool(y,curr)
                    | DataTypes.OR(x,y) => check_bool(x,curr) andalso check_bool(y,curr)

                    | DataTypes.RPLUS(x,y) => check_rat(x,curr) andalso check_rat(y,curr)
                    | DataTypes.RMINUS(x,y) => check_rat(x,curr) andalso check_rat(y,curr)
                    | DataTypes.RTIMES(x,y) => check_rat(x,curr) andalso check_rat(y,curr)
                    | DataTypes.RDIV(x,y) => check_rat(x,curr) andalso check_rat(y,curr)
                    | DataTypes.INV(x) => check_rat(x,curr)
                    | DataTypes.MAKE(x,y) => check_int(x,curr) andalso check_int(y,curr)
                    | DataTypes.RAT(x) => check_int(x,curr)

                    | DataTypes.PLUS(x,y) => check_int(x,curr) andalso check_int(y,curr)
                    | DataTypes.MINUS(x,y) => check_int(x,curr) andalso check_int(y,curr)
                    | DataTypes.TIMES(x,y) => check_int(x,curr) andalso check_int(y,curr)
                    | DataTypes.DIV(x,y) => check_int(x,curr) andalso check_int(y,curr)
                    | DataTypes.MOD(x,y) => check_int(x,curr) andalso check_int(y,curr)

                    | DataTypes.NEG(x) => check_rat(x,curr) orelse check_int(x,curr)
                    | DataTypes.GEQ(x,y) => (check_int(x,curr) andalso check_int(y,curr)) orelse (check_rat(x,curr) andalso check_rat(y,curr))
                    | DataTypes.LEQ(x,y) => (check_int(x,curr) andalso check_int(y,curr)) orelse (check_rat(x,curr) andalso check_rat(y,curr))
                    | DataTypes.LT(x,y) => (check_int(x,curr) andalso check_int(y,curr)) orelse (check_rat(x,curr) andalso check_rat(y,curr))
                    | DataTypes.GT(x,y) => (check_int(x,curr) andalso check_int(y,curr)) orelse (check_rat(x,curr) andalso check_rat(y,curr))

                    | DataTypes.EQ(x,y) => (check_int(x,curr) andalso check_int(y,curr)) orelse (check_rat(x,curr) andalso check_rat(y,curr)) orelse (check_bool(x,curr) andalso check_bool(y,curr))
                    | DataTypes.NEQ(x,y) => (check_int(x,curr) andalso check_int(y,curr)) orelse (check_rat(x,curr) andalso check_rat(y,curr)) orelse (check_bool(x,curr) andalso check_bool(y,curr))

                    | DataTypes.Identifier(id) => find_var(id,curr)
                    | DataTypes.TT => true
                    | DataTypes.FF => true
                    | DataTypes.Ratnumeral(x) => true
                    | DataTypes.Intnumeral(x) => true

        fun processBLK(node) = 
        let val DataTypes.BLK(dec,comseq) = node
            val DataTypes.DEC(var,proc) = dec
            val DataTypes.Vardec(rvar,ivar,bvar) = var
            val table : (string, DataTypes.Type) HashTable.hash_table = HashTable.mkTable (HashString.hashString, op=) (42, Fail "Yaar kyu ho rha hai ye sb\n");
            val ratvar = inserttype(#1 rvar,#2 rvar,table)
            val intvar = inserttype(#1 ivar,#2 ivar,table)
            val boolvar = inserttype(#1 bvar,#2 bvar,table)
            val _ = HashTable.insert typetable(!scope,table)
            val curr = !scope
            val _ = processPROC (proc,curr)
            val _ = processCMD (comseq,curr)
        in HashTable.insert cmdlist (curr,comseq)
        end

        and processPROC([],curr) = ()
            | processPROC(p::ps,curr) = 
            let val DataTypes.Procdef(id,blk) = p
                val _ = increment scope
                val _ = if(isSome(HashTable.find procedure id)) then raise ProcedureRedeclaration 
                        else (HashTable.insert procedure (id,!scope);HashTable.insert parent (!scope,curr))
            in (processBLK(blk); processPROC(ps,curr))
            end

        and processCMD([],curr) = ()
        | processCMD(cmd::cmds,curr) = 
        (case cmd of DataTypes.READ(x) => (if(find_var(x,curr)) then () else raise VariableNotDeclared)
                    | DataTypes.CALL(x) => (if(isSome(HashTable.find procedure x)) then () else raise ProcedureNotDeclared)
                    | DataTypes.PRINT(x) => (if(valid_expr(x,curr)) then () else raise InvalidExpression)
                    | DataTypes.ASSIGN(id,x) => 
                    if(valid_expr(x,curr)) then
                    let val t = findtype(id,curr)
                    in case t of SOME DataTypes.bigint => (if check_int(x,curr) then () else raise ExpressionTypeDoesNotMatchVariableType)
                                | SOME DataTypes.rational => (if check_rat(x,curr) then () else raise ExpressionTypeDoesNotMatchVariableType)
                                | SOME DataTypes.Bool => ( if check_bool(x,curr) then () else raise ExpressionTypeDoesNotMatchVariableType)
                                | NONE => raise VariableNotDeclared
                    end
                    else raise InvalidExpression
                    | DataTypes.ITE(x,cm1,cm2) =>
                    (if check_bool(x,curr) then (processCMD (cm1,curr);processCMD (cm2,curr)) else raise NotABooleanExpression)
                    | DataTypes.WHILE(x,cm) =>
                    (if check_bool(x,curr) then (processCMD (cm,curr)) else raise NotABooleanExpression);
            processCMD(cmds,curr))

        fun eval_bool(term,curr) = 
        case term of DataTypes.NOT(x) => not(eval_bool(x,curr))
                    | DataTypes.AND(x,y) => eval_bool(x,curr) andalso eval_bool(y,curr)
                    | DataTypes.OR(x,y) => eval_bool(x,curr) orelse eval_bool(y,curr)

                    | DataTypes.GEQ(x,y) => if check_int(x,curr) then BigInt.grt(eval_int(x,curr),eval_int(y,curr)) else not(Rational.less(eval_rat(x,curr),eval_rat(y,curr)))
                    | DataTypes.LEQ(x,y) => if check_int(x,curr) then BigInt.grt(eval_int(y,curr),eval_int(x,curr)) else not(Rational.less(eval_rat(y,curr),eval_rat(x,curr)))
                    | DataTypes.LT(x,y) => if check_int(x,curr) then not(BigInt.grt(eval_int(x,curr),eval_int(y,curr))) else Rational.less(eval_rat(x,curr),eval_rat(y,curr))
                    | DataTypes.GT(x,y) => if check_int(x,curr) then not(BigInt.grt(eval_int(y,curr),eval_int(x,curr)))else Rational.less(eval_rat(y,curr),eval_rat(x,curr))
                    | DataTypes.EQ(x,y) => if check_bool(x,curr) then eval_bool(x,curr) = eval_bool(y,curr) else if check_int(x,curr) then BigInt.equal(eval_int(x,curr),eval_int(y,curr)) else Rational.equal(eval_rat(x,curr),eval_rat(y,curr))
                    | DataTypes.NEQ(x,y) => if check_bool(x,curr) then eval_bool(x,curr) <> eval_bool(y,curr) else if check_int(x,curr) then not(BigInt.equal(eval_int(x,curr),eval_int(y,curr)))else not(Rational.equal(eval_rat(x,curr),eval_rat(y,curr)))
                    | DataTypes.TT => true
                    | DataTypes.FF => false
                    | DataTypes.Identifier(id) => let val SOME(s) = findscope(id,curr) in HashTable.lookup (HashTable.lookup booltable s) id end
                    | _ => raise TypeMisMatchException
                    
        and eval_int(term,curr) = 
        case term of DataTypes.PLUS(x,y) => BigInt.add(eval_int(x,curr),eval_int(y,curr))
                    | DataTypes.MINUS(x,y) => BigInt.sub(eval_int(x,curr),eval_int(y,curr))
                    | DataTypes.TIMES(x,y) => BigInt.mul(eval_int(x,curr),eval_int(y,curr))
                    | DataTypes.DIV(x,y) => BigInt.division(eval_int(x,curr),eval_int(y,curr))
                    | DataTypes.MOD(x,y) => BigInt.modulo(eval_int(x,curr),eval_int(y,curr))
                    | DataTypes.NEG(x) => BigInt.neg(eval_int(x,curr))
                    | DataTypes.Intnumeral(x) => x
                    | DataTypes.Identifier(id) => let val SOME(s) = findscope(id,curr) in HashTable.lookup (HashTable.lookup inttable s) id end
                    | _ => raise TypeMisMatchException

        and eval_rat(term,curr) = 
        case term of DataTypes.RPLUS(x,y)=> Rational.add(eval_rat(x,curr),eval_rat(y,curr))
                    | DataTypes.RMINUS(x,y)=> Rational.subtract(eval_rat(x,curr),eval_rat(y,curr))
                    | DataTypes.RTIMES(x,y)=> Rational.multiply(eval_rat(x,curr),eval_rat(y,curr))
                    | DataTypes.RDIV(x,y)=> let val res = Rational.divide(eval_rat(x,curr),eval_rat(y,curr)) in if isSome(res) then valOf(res) else raise DivisionByZero end
                    | DataTypes.NEG(x)=> Rational.neg(eval_rat(x,curr))
                    | DataTypes.INV(x) => 
                    let val inv = Rational.inverse(eval_rat(x,curr))
                    in if(isSome(inv)) then valOf(inv) else raise DivisionByZero
                    end
                    | DataTypes.RAT(x) =>
                    let val rat = Rational.rat(eval_int(x,curr)) in valOf(rat) end
                    | DataTypes.MAKE(x,y) => 
                    let val mrat = Rational.make_rat(eval_int(x,curr),eval_int(y,curr))
                    in if(isSome(mrat)) then valOf(mrat) else raise DivisionByZero
                    end
                    | DataTypes.Ratnumeral(x) => x
                    | DataTypes.Identifier(id) => let val SOME(s) = findscope(id,curr) in HashTable.lookup (HashTable.lookup rattable s) id end
                    | _ => raise TypeMisMatchException

        fun run_commands(output_file,[],curr) = ()
        | run_commands(output_file,cmd::cmds,curr) = 
            case cmd of DataTypes.READ(x) => 
                        let val SOME(t) = findtype(x,curr)
                            val input = getInput("Enter the value of "^x)
                            val SOME(real) = findscope(x,curr)
                        in (case t of DataTypes.Bool => 
                                    let val table : (string,bool) HashTable.hash_table
                                                = if(isSome(HashTable.find booltable real)) then valOf(HashTable.find booltable real) 
                                                else HashTable.mkTable (HashString.hashString, op=) (42, Fail "Boolean declare nhi kara\n");
                                        val _ = if input = "tt" then HashTable.insert table (x,true) 
                                                else if input = "ff" then HashTable.insert table (x,false) 
                                                else raise NotBoolean
                                    in HashTable.insert booltable (real,table)
                                    end
                                    | DataTypes.bigint => 
                                    let val table : (string,BigInt.bigint) HashTable.hash_table
                                                = if(isSome(HashTable.find inttable real)) then valOf(HashTable.find inttable real) 
                                                else HashTable.mkTable (HashString.hashString, op=) (42, Fail "Integer declare nhi kara\n");
                                        val _ = HashTable.insert table (x,BigInt.fromString(input))
                                    in HashTable.insert inttable (real,table)
                                    end
                                    | DataTypes.rational => 
                                    let val table : (string,Rational.rational) HashTable.hash_table
                                                = if(isSome(HashTable.find rattable real)) then valOf(HashTable.find rattable real) 
                                                else HashTable.mkTable (HashString.hashString, op=) (42, Fail "Rational declare nhi kara\n");
                                        val _ = HashTable.insert table (x,Rational.fromDecimal(input))
                                    in HashTable.insert rattable (real,table)
                                    end; run_commands(output_file,cmds,curr))
                        end
                        | DataTypes.CALL(x) => 
                        let val no = HashTable.lookup procedure x
                            val subcmds = HashTable. lookup cmdlist no
                        in (run_commands(output_file,subcmds,no);run_commands(output_file,cmds,curr))
                        end
                        | DataTypes.PRINT(x) =>
                        (if check_bool(x,curr) then if eval_bool(x,curr) then TextIO.output(output_file,"tt\n") else TextIO.output(output_file,"ff\n")
                        else if check_int(x,curr) then TextIO.output(output_file,BigInt.toString(eval_int(x,curr))^"\n")
                        else TextIO.output(output_file,Rational.toDecimal(eval_rat(x,curr))^"\n"); run_commands(output_file,cmds,curr))
                        | DataTypes.ASSIGN(id,x) =>
                        let val SOME(t) = findtype(id,curr)
                            val SOME(real) = findscope(id,curr)
                        in (case t of DataTypes.Bool => 
                                    let val table : (string,bool) HashTable.hash_table
                                                = if(isSome(HashTable.find booltable real)) then valOf(HashTable.find booltable real) 
                                                else HashTable.mkTable (HashString.hashString, op=) (42, Fail "Boolean declare nhi kara\n");
                                        val _ = HashTable.insert table (id,eval_bool(x,curr))
                                    in (HashTable.insert booltable (real,table))
                                    end
                                    | DataTypes.bigint => 
                                    let val table : (string,BigInt.bigint) HashTable.hash_table
                                                = if(isSome(HashTable.find inttable real)) then valOf(HashTable.find inttable real) 
                                                else HashTable.mkTable (HashString.hashString, op=) (42, Fail "Integer declare nhi kara\n");
                                        val _ = HashTable.insert table (id,eval_int(x,curr))
                                    in (HashTable.insert inttable (real,table))
                                    end
                                    | DataTypes.rational => 
                                    let val table : (string,Rational.rational) HashTable.hash_table
                                                = if(isSome(HashTable.find rattable real)) then valOf(HashTable.find rattable real) 
                                                else HashTable.mkTable (HashString.hashString, op=) (42, Fail "Rational declare nhi kara\n");
                                        val _ = HashTable.insert table (id,eval_rat(x,curr))
                                    in (HashTable.insert rattable (real,table))
                                    end; run_commands(output_file,cmds,curr))
                        end
                        | DataTypes.ITE(x,cm1,cm2) => (if(eval_bool(x,curr)) then run_commands(output_file,cm1,curr) else run_commands(output_file,cm2,curr);run_commands(output_file,cmds,curr))
                        | DataTypes.WHILE(x,cm) => if(eval_bool(x,curr)) then (run_commands(output_file,cm,curr);run_commands(output_file,cmd::cmds,curr)) else run_commands(output_file,cmds,curr)

        val ast = compile(inputFile)
        val output_file = TextIO.openOut(outputFile)
        val DataTypes.START(blk) = ast;
        val _ = HashTable.clear typetable;
        val _ = HashTable.clear rattable;
        val _ = HashTable.clear inttable;
        val _ = HashTable.clear booltable;
        val _ = HashTable.clear procedure;
        val _ = HashTable.clear parent;
        val _ = processBLK(blk);
        val commands = if isSome(HashTable.find (cmdlist) (0)) then valOf(HashTable.find (cmdlist) (0)) else [];
        val _ = run_commands(output_file,commands,0);
    in TextIO.closeOut output_file
    end
end;

