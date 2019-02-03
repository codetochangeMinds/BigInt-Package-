structure BigNat :> BigNat =
  struct
    type bignat			= int list	(* least significant first *)

    val base			= 10000		(* must be <=sqrt(Int.maxInt) *)

    fun fromInt 0		= []
      | fromInt n		= if n < 0 then raise Domain
      				  else n mod base :: fromInt(n div base)
    fun toInt  []		= 0
      | toInt(d::ds)		= d + base * toInt ds	(* may raise Overflow *)

    fun pad s			= if String.size s = 4 then s else pad("0" ^ s)
    fun toString ds		= case List.rev ds
				    of  []   => "0"
				     | d::dr => String.concat(Int.toString d ::
			                       List.map (pad o Int.toString) dr)

    infixr 5 :::		(* cons that discards `leading zeros' *)
    fun 0:::[]			= []
      | n:::ns			= n::ns

    fun isEven  []		= true
      | isEven(a::ar)		= a mod 2 = 0

    fun add(ar,    [],  0)	= ar
      | add([],    br,  0)	= br
      | add([],    [],  c)	= [c]
      | add(ar,    [],  c)	= add(ar,[0],c)
      | add([],    br,  c)	= add([0],br,c)
      | add(a::ar,b::br,c)	= let val (d,c') = if a+b+c < base
				  		   then (a+b+c, 0)
						   else (a+b+c-base, 1)
				  in
				      d :: add(ar,br,c')
				  end

    fun sub ... (* INSERT CODE HERE *)

    fun x + y			= add(x,y,0)
    fun x - y			= sub(x,y,0)


    fun mul( x,    0)		= []
      | mul( [],   b)		= []
      | mul(a::ar, b)		= fromInt(a*b) + (0::mul(ar,b))

    fun [] * y			= []
      | x  * []			= []
      | x  * (b::br)		= mul(x,b) + (0::(x*br))

    fun x <= y			= (sub(y,x,0) ; true) handle Domain => false
    fun x < y			= x <> y andalso x <= y

    fun divmod(x,[])		= raise Div
      | divmod(x,y)		= if x < y then ([],x) else
				  let fun dm(x,y,c) = if x < y then (x,c)
						      else dm(x-y,y,Int.+(c,1))
				      val (d,m)  = divmod(x,0::y)
				      val (m',c) = dm(m,y,0)
				  in
				      (c:::d, m')
				  end

    fun x div y			= #1 (divmod(x,y))
    fun x mod y			= #2 (divmod(x,y))

    fun exp ... (* INSERT CODE HERE *)

  end
