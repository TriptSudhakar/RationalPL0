signature BIGINT = 
sig
type bigint
val fromInt: int -> bigint
val toString: bigint -> string
val fromString: string -> bigint
val neg: bigint -> bigint
val abs: bigint -> bigint
val grt: bigint * bigint -> bool
val equal: bigint * bigint -> bool
val add: bigint * bigint -> bigint
val sub: bigint * bigint -> bigint
val mul: bigint * bigint -> bigint
val division: bigint * bigint -> bigint
val modulo: bigint * bigint -> bigint
val gcd: bigint * bigint -> bigint
val long_division: bigint * bigint -> string
end

structure BigInt:BIGINT =
struct
    type bigint = int * int list

    fun fromInt_helper(0:int) = [0]
      | fromInt_helper(x:int) = if x<10 then [x] else (x mod 10) :: fromInt_helper(x div 10)

    fun fromInt(x) = if x<0 then (1,fromInt_helper(~x)) else (0,fromInt_helper(x))
    
    fun neg((s,x):bigint) = (1-s,x)
    fun abs((s,x):bigint) = (0,x)

    fun remove_lead0([]) = [0] 
      | remove_lead0(x::xs) = if x = 0 then remove_lead0(xs) else x::xs

    fun str([]) = []
      | str(x::xs) = Char.chr(x+Char.ord(#"0"))::str(xs)
    
    fun toString(x:bigint) = if (#1 x) = 1 then String.implode(#"~"::rev(str(rev(remove_lead0(rev(#2 x)))))) else String.implode(rev(str(rev(remove_lead0(rev(#2 x))))))

    fun fstr_helper([]) = []
      | fstr_helper(x::xs) = (Char.ord(x) - Char.ord(#"0")) :: fstr_helper(xs)
    fun fromString(x:string) = 
    if x = "" then (0,[0]) 
    else 
        let val l = String.explode(x)
        in if(hd(l)) = #"~" then (1,rev(fstr_helper(tl(l)))) else (0,rev(fstr_helper(l)))
    end
    
    fun compare_helper([],[]) = true
      | compare_helper(x::xs,y::ys) = x>y orelse (x=y andalso compare_helper(xs,ys))

    fun compare(x:int list,y:int list) = 
        let val nx = remove_lead0(rev(x))
            val ny = remove_lead0(rev(y))
        in if length(nx) <> length(ny) then length(nx)>length(ny) else compare_helper(nx,ny)
        end

    fun grt((0,x):bigint,(0,y):bigint) = compare(x,y)
      | grt((1,x):bigint,(1,y):bigint) = compare(y,x)
      | grt((1,x):bigint,(0,y):bigint) = not (grt((0,x),(1,y)))
      | grt((0,x):bigint,(1,y):bigint) = if compare(x,[0]) andalso compare(y,[0]) then true else false
    
    fun equal(x:bigint,y:bigint) = grt(x,y) andalso grt(y,x)

    fun add_helper(x:int list,y:int list,c:int) = 
        if null x andalso null y then if c = 1 then [1] else []
        else if null y then
            if c = 0 then x else 
            let val r = 1 + hd(x)
                val d = r mod 10
                val cn = r div 10
            in d::add_helper(tl(x),[],cn)
            end
        else if null x then add_helper(y,x,c)
        else
            let val r = c + hd(x) + hd(y)
                val d = r mod 10
                val cn = r div 10
            in d::add_helper(tl(x),tl(y),cn)
            end
    
    fun sub_helper(x:int list,y:int list,b:int) = 
        if null x andalso null y then []
        else if null y then
            if b = 0 then x else 
            let val r = hd(x) - b
                val d = if r<0 then 10+r else r
                val bn = if r<0 then 1 else 0
            in  d::sub_helper(tl(x),[],bn)
            end
        else
            let val r = hd(x) - hd(y) - b
                val d = if r<0 then 10+r else r
                val bn = if r<0 then 1 else 0
            in d::sub_helper(tl(x),tl(y),bn)
            end
    
    fun add((0,x):bigint,(0,y):bigint) = (0,add_helper(x,y,0))
      | add((1,x):bigint,(1,y):bigint) = (1,add_helper(x,y,0))
      | add((1,x):bigint,(0,y):bigint) = 
        let val grt = compare(y,x) in if grt then (0,sub_helper(y,x,0)) else (1,sub_helper(x,y,0)) end
      | add((0,x):bigint,(1,y):bigint) = 
        let val grt = compare(x,y) in if grt then (0,sub_helper(x,y,0)) else (1,sub_helper(y,x,0)) end

    fun sub(x:bigint,y:bigint) = add((#1 x,#2 x),(1 - (#1 y),#2 y))

    fun mul_digit(x:int list,i:int,c:int) = 
        if i = 0 then [0]
        else if null x andalso c = 0 then []
        else if null x then (c mod 10)::mul_digit([],i,c div 10)
        else
        let val r = c + hd(x)*i
            val d = r mod 10
            val cn = r div 10
        in  d::mul_digit(tl(x),i,cn)
        end
            
    fun mul_helper([]:int list,[]:int list) = []
      | mul_helper(x:int list,[]:int list) = []
      | mul_helper(x:int list,y::ys:int list) = 
        if y = 0 then mul_helper(0::x,ys)
        else 
        let val d = mul_digit(x,y,0)
        in add_helper(d,mul_helper(0::x,ys),0)
        end

    fun mul(x:bigint,y:bigint) = 
    let val nx = rev(remove_lead0(rev(#2 x)))
        val ny = rev(remove_lead0(rev(#2 y)))
        val sign = if #1 x = #1 y then 0 else 1
    in if length(ny)>length(nx) then (sign,mul_helper(ny,nx)) else (sign,mul_helper(nx,ny))
    end 

    fun increment(no:int list,d:int list,inc:int) = 
    let val prod = mul_digit(d,inc,0)
    in 
        if inc = 10 then 9
        else if not(compare(no,prod)) then inc - 1
        else increment(no,d,inc+1)
    end

    fun div_helper(x:int list,[]:int list,d:int list) = 
        let val inc = increment(rev(x),d,1)
            val dvd = remove_lead0(rev(sub_helper(rev(x),mul_digit(d,inc,0),0)))
        in  ([inc],dvd)
        end
      | div_helper(x:int list,y::ys:int list,d:int list) = 
        let val inc = increment(rev(x),d,1)
            val dvd = remove_lead0(rev(sub_helper(rev(x),mul_digit(d,inc,0),0)))
            val divide = div_helper(rev(y::rev(dvd)),ys,d)
        in  (inc::(#1 divide),#2 divide)
        end

    fun division(x:bigint,y:bigint) = 
    let val sign = if #1 x = #1 y then 0 else 1
        val non_zero = compare(#2 x,#2 y)
        val dvd = remove_lead0(rev(#2 x))
    in if non_zero then (sign,rev(remove_lead0(#1 (div_helper([hd(dvd)],tl(dvd),rev(remove_lead0(rev(#2 y)))))))) else (sign,[0]) 
    end

    fun modulo(x:bigint,y:bigint) = 
    let val sign = if #1 x = 0 then 0 else 1
        val non_zero = compare(#2 x,#2 y)
        val dvd = remove_lead0(rev(#2 x))
    in if non_zero then (sign,rev(remove_lead0(#2 (div_helper([hd(dvd)],tl(dvd),rev(remove_lead0(rev(#2 y)))))))) else (sign,rev(remove_lead0(rev(#2 x))))
    end

    fun gcd(x:bigint,y:bigint) = 
    if equal(x,fromInt(0)) then if equal(y,fromInt(0)) then fromInt(1) else y
    else if equal(y,fromInt(0)) then x
    else gcd(y,modulo(x,y))

    fun found(e,[]) = NONE
      | found(e,(x,i)::xs) = if equal((0,e),(0,x)) then SOME (x,i) else found(e,xs)

    fun c_split(0,x::xs) = ([],x::xs)
      | c_split(c,x::xs) = let val t = c_split(c-1,xs) in (x::(#1 t),#2 t) end

    fun pr([]) = "\n"
      | pr(x::xs:(int list * int) list) = "("^String.implode(str(#1 x))^","^Int.toString(#2 x)^") " ^ pr(xs)

    fun long_div_helper(x:int list,y:int list,rem_list:(int list*int) list,i:int,q:int list) =
    if equal((0,x),(0,[0])) then ("t",[0],rem_list,q)
    else if isSome(found(x,rem_list)) then ("r",x,rem_list,q)
    else
        let val inc = increment(0::x,y,1)
            val prod = mul_digit(y,inc,0)
            val new_x = (sub((0,0::x),(0,prod)))
        in long_div_helper(rev(remove_lead0(rev(#2 new_x))),y,(x,i)::rem_list,i+1,rev(inc::rev(q)))
        end
    
    fun long_division(x:bigint,y:bigint) = 
    let val i_part = division(x,y)
        val strw = toString(i_part)
        val rem = modulo(abs(x),y)
        val (ty,r,rem_list,q) = long_div_helper(#2 rem,#2 y,[],0,[])
        val _ = pr(rem_list)
    in 
        if ty = "t" then strw^"."^String.implode(str(q))^"(0)"
        else 
            let val split = #2 (valOf(found(r,rem_list)))
                val (nr,r) = c_split(split,q)
            in strw^"."^String.implode(str(nr))^"("^String.implode(str(r))^")"
            end
    end
end

signature RATIONAL =
sig
type bigint
type rational
exception rat_error
val make_rat: bigint * bigint -> rational option
val rat: bigint -> rational option
val reci: bigint -> rational option
val neg: rational -> rational
val inverse : rational -> rational option
val equal : rational * rational -> bool (* equality *)
val less : rational * rational -> bool (* less than *)
val add : rational * rational -> rational (* addition *)
val subtract : rational * rational -> rational (* subtraction *)
val multiply : rational * rational -> rational (* multiplication *)
val divide : rational * rational -> rational option (* division *)
val showRat : rational -> string
val fromDecimal : string -> rational
val showDecimal : rational -> string
val toDecimal : rational -> string
end

functor Rational (BigInt : BIGINT) : RATIONAL =
struct
    type bigint = BigInt.bigint
    type rational = bigint * bigint
    exception rat_error 
    fun make_rat(x:BigInt.bigint,y:BigInt.bigint):rational option = 
    if BigInt.equal(y,BigInt.fromInt(0)) then NONE 
    else if  BigInt.grt(BigInt.fromInt(0),y) then make_rat(BigInt.neg(x),BigInt.neg(y)) 
    else 
        let val z = BigInt.gcd(BigInt.abs(x),BigInt.abs(y))
        in SOME (BigInt.division(x,z),BigInt.division(y,z)) 
        end
    fun rat(x:BigInt.bigint):rational option = SOME (x,BigInt.fromInt(1))
    fun reci(x:BigInt.bigint):rational option = if BigInt.equal(x,BigInt.fromInt(0)) then NONE else SOME (x,BigInt.fromInt(1))
    fun neg(x:rational) = (BigInt.neg(#1 x),#2 x)
    fun inverse(x:rational) = make_rat(#2 x,#1 x)
    fun equal(x:rational,y:rational) = BigInt.equal(BigInt.mul(#1 x,#2 y),BigInt.mul(#1 y,#2 x))
    fun less(x:rational,y:rational) = not(BigInt.grt(BigInt.mul(#1 x,#2 y),BigInt.mul(#1 y,#2 x)))
    fun add(x:rational,y:rational) = valOf(make_rat(BigInt.add(BigInt.mul(#1 x,#2 y),BigInt.mul(#1 y,#2 x)),BigInt.mul(#2 x,#2 y)))
    fun subtract(x:rational,y:rational) = valOf(make_rat(BigInt.sub(BigInt.mul(#1 x,#2 y),BigInt.mul(#1 y,#2 x)),BigInt.mul(#2 x,#2 y)))
    fun multiply(x:rational,y:rational) = valOf(make_rat(BigInt.mul(#1 x,#1 y),BigInt.mul(#2 x,#2 y)))
    fun divide(x:rational,y:rational) = let val z = inverse(y) in if isSome(z) then SOME (multiply(x,valOf(z))) else NONE end
    fun showRat(x:rational) = BigInt.toString(#1 x)^"/"^BigInt.toString(#2 x)

    local 
        fun segment(ch:char,[]:char list) = ([],[])
          | segment(ch:char,x::xs:char list) = 
            if x = ch then ([],xs) else let val sub = segment(ch,xs) in (x::(#1 sub),#2 sub) end

        fun repeat(ch:char,i:int) = if i = 0 then [] else ch::repeat(ch,i-1)
    in 
        fun fromDecimal(x:string) = 
        let val l = String.explode(x)
            val i = segment(#".",l)
            val nr = segment(#"(",(#2 i))
            val r = rev(tl(rev(#2 nr)))
            val den = BigInt.fromString(String.implode(repeat(#"9",length(r)) @ repeat(#"0",length(#1 nr))))
            val s = if null (#1 i) then 0 else if hd(#1 i) = #"~" then 1 else 0
            val w = if null (#1 i) then [#"0"] else if hd(#1 i) = #"~" orelse hd(#1 i) = #"+" then tl(#1 i) else (#1 i)
            val num = BigInt.sub(BigInt.fromString(String.implode((#1 nr)@r)),BigInt.fromString(String.implode((#1 nr))))
            val ans = add(valOf(rat(BigInt.fromString(String.implode(w)))),valOf(make_rat(num,den)))
        in if s = 1 then neg(ans) else ans
        end
    end
    fun showDecimal(x:rational) = BigInt.long_division(#1 x,#2 x)
    fun toDecimal(x:rational) = showDecimal(x)
end

structure Rational = Rational(BigInt);