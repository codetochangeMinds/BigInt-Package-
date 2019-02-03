signature BIGNAT =
sig
type bignat
exception overflow
exception underflow
(*zero is constructor of bignat*)
val zero : bignat
val normalize : bignat -> bignat
val fromString : string -> bignat
val toString : bignat -> string
val ++ : bignat * bignat -> bignat
val succ : bignat -> bignat
val min : bignat * bignat -> bignat
val max : bignat * bignat -> bignat
val ** : bignat * bignat -> bignat
(*val compare : bignat * bignat -> order*)
val << : bignat * bignat -> bool
val <<= : bignat * bignat -> bool
val >> : bignat * bignat -> bool
val >>= : bignat * bignat -> bool
val == : bignat * bignat -> bool
val len : bignat -> int
(*val lenCompare : bignat * bignat -> order*)
val lenLt : bignat * bignat -> bool
val lenLeq : bignat * bignat -> bool
val lenGt : bignat * bignat -> bool
val lenGeq : bignat * bignat -> bool
val lenEq : bignat * bignat -> bool
val -- : bignat * bignat -> bignat
val pred : bignat -> bignat
(*exception division_by_zero
when to give emptyList exception
exception emptyList
val %% : bignat * bignat -> bignat * bignat
val quo : bignat * bignat -> bignat
val rem : bignat * bignat -> bignat*)
end

structure Bignat:BIGNAT= 
struct
	(* Body *)
	type bignat= int list;

	exception overflow;
	exception underflow;

	val zero=[];

	fun normalize nil=raise underflow 
			| normalize (h::t)=
			if(h=0) then normalize(t)
			else h::t;


	fun fromString s = 
			let 
			(*apply map of ord*)
				fun map ord [] = [] 
					| map ord (h::t) = (ord h - 48)::(map ord t)
				val (h::t)=explode(s);
			in
				map ord (h::t)
			end 
	;

	fun toStr (h::t)=
		let fun map chr []=[]
			| map chr (h::t)=(chr(48+h)::(map chr t))
		in map chr (h::t)
		end
	fun toString []=raise underflow
	|	toString (h::t)= implode(toStr(h::t));

	infix ++;
	fun []++[]=raise underflow
	| l1++[]=l1
	|[]++l1=l1
	| l1++l2=
	let val h1::t1=rev(l1);
		val hd1::tl1=rev(l2)
		fun ad([],[],carry)=[carry]
		|   ad([],h::t,carry)=(h+carry)mod 10::ad([],t,(h+carry) div 10)
		|   ad(h::t,[],carry)=(h+carry)mod 10::ad(t,[],(h+carry) div 10)
		| 	ad(h::t,h1::t1,carry)=
									(h+h1+carry)mod 10::ad(t,t1,(h+h1+carry) div 10)
		in
			rev(ad(h1::t1,hd1::tl1,0))
	end;

	fun succ [] = []++[1]
	|	succ l1 = l1++[1];


	fun min ([],[])=raise underflow
	| min([],h::t)=nil
	| min(h::t,[])=nil
	| min(l1,l2)=
	let 
		val l3=normalize(l1)
		val l4=normalize(l2)

	in
		if(length(l3)=length(l4)) then 
			(
				if(hd(l3)=hd(l4))	then min(tl(l3),tl(l4))
				else if(hd(l3)<hd(l4))	then l3
				else l4
			)
		else if(length(l3)<length(l4))	then l3
		else l4
	end;

	fun max ([],[])=raise underflow
	| max([],h::t)=h::t
	| max(h::t,[])=h::t
	| max(l1,l2)=
	let 
		val l3=normalize(l1)
		val l4=normalize(l2)

	in
		if(length(l3)=length(l4)) then 
			(
				if(hd(l3)=hd(l4))	then min(tl(l3),tl(l4))
				else if(hd(l3)>hd(l4))	then l3
				else l4
			)
		else if(length(l3)>length(l4))	then l3
		else l4
	end;


	infix **;
	fun []**[]=raise underflow
	|	l1**[]=raise underflow
	| 	[]**l1=raise underflow
	| 	l1**l2=
		let val l3=rev(l1);
			val	l4=rev(l2)
			fun mul([],p,carry)=[carry]
			| 	mul(h1::t1,p,carry)=
								((h1*p+carry) mod 10):: mul(t1,p,((h1*p)+carry) div 10)
			fun mulRes([],[],carry)=[]
			| 	mulRes(l1,[],carry)=[0]
			|	mulRes([],l1,carry)=[]
			|  	mulRes(l1,l2,carry)=
									rev(mul(l1,hd(l2),carry))++(mulRes(0::l1,tl(l2),0))

		in normalize(mulRes(l3,l4,0))
		end


	(*fun compare*)

	infix <<;
	fun [] << []=false	
	| [] << l1=true
	| l1 << []=false
	| l1 << l2=
	if(max(l1,l2)=l2) then true
	else false

	fun equal([],[])=true
	| equal([],h::t)=false
	| equal(h::t,[])=false
	| equal(l1,l2)=
	let 
		val l3=normalize(l1)
		val l4=normalize(l2)
	in
		if(length(l3)=length(l4)) then 
			(
				if(hd(l3)=hd(l4))	then equal(tl(l3),tl(l4))
				else false
			)
		else if(length(l3)<length(l4))	then false
		else false
	end;


	infix<<=;
	fun []<<=[]=true
	| []<<=l1=false
	| l1<<=[]=true
	| l1<<=l2=
	if(equal(l1,l2)) then true
	else if(max(l1,l2)=l2) then true
	else false

	infix>>
	fun []>>[]=false
	| []>>l1=true
	| l1>>[]=false
	| l1>>l2=
	if(max(l1,l2)=l1) then true
	else false

	infix>>=
	fun []>>=[]=true
	| []>>=l1=true
	| l1>>=[]=false
	| l1>>=l2=
	if(equal(l1,l2)) then true
	else if(max(l1,l2)=l1) then true
	else false

	infix==
	fun []==[]=false
	| []==l1=false
	| l1==[]=false
	| l1==l2=
	if(equal(l1,l2)) then true
	else false

	fun len([])=0
	| len(l1)=length(l1);

	(*fun lenCompare 
		how to use order datatype*)

	fun lenLt([],[])=false
	| 	lenLt([],h::t)=false
	| 	lenLt(h::t,[])=false
	| 	lenLt(l1,l2)=
		if(length(l1)<length(l2)) then true
			else false

	fun lenLeq([],[])=true
	| 	lenLeq([],h::t)=true
	| 	lenLeq(h::t,[])=false
	| 	lenLeq(l1,l2)=
		if(length(l1)<=length(l2)) then true
			else false	

	fun lenGt([],[])=false
	| 	lenGt([],h::t)=false
	| 	lenGt(h::t,[])=true
	| 	lenGt(l1,l2)=
		if(length(l1)>length(l2)) then true
			else false	

	fun lenGeq([],[])=false
	| 	lenGeq([],h::t)=false
	| 	lenGeq(h::t,[])=true
	| 	lenGeq(l1,l2)=
		if(length(l1)>=length(l2)) then true
			else false	

	fun lenEq([],[])=true
	| 	lenEq([],h::t)=false
	| 	lenEq(h::t,[])=false
	| 	lenEq(l1,l2)=
		if(length(l1)=length(l2)) then true
		else false	


	infix --;
	fun []--[]=raise underflow
		|l1--[]=l1
		|[]--l1=raise underflow
		|l1--l2=
		let val h1::t1=rev(l1);
			val h2::t2=rev(l2)
			fun sub([],[])=[]
				| sub(l1,[])=l1
				| sub([],l1)=raise underflow
				| sub(h2::t2,h3::t3)=
					if(h2::t2>>=h3::t3) then	
					(
						if(h2<h3) then 
						(
							if(hd(t2)>1)
								then (10+h2-h3)::sub(hd(t2)-1::tl(t2),t3)
							else (10+h2-h3)::sub(9::tl(t2),t3)
						)
						else if(h2>h3)  then (h2-h3)::sub(t2,t3)
						else (0)::sub(t2,t3)
					)
					else raise underflow
		in 
		rev(sub(h1::t1,h2::t2))	
		end;

		fun pred [] = raise underflow
		|	pred l1 = l1--[1];

end

	
	functor BigInt (Bn:BIGNAT):
		sig
		type bigint
		val bigzero: bigint
		val normalize : bigint -> bigint
		(*val bigint: int -> bigint
		val fromString : string -> bigint option*)
		val toString : bigint -> string
		(*val bigint : int -> bigint*
		val int : bigint -> int option*)
		val ~~ : bigint -> bigint
		val abs : bigint -> bigint
		val ++ : bigint * bigint -> bigint
		val succ : bigint -> bigint
		val min : bigint * bigint -> bigint
		val max : bigint * bigint -> bigint
		val sign : bigint -> int
		val sameSign : bigint * bigint -> bool
		val ** : bigint * bigint -> bigint
		(*val compare : bigint * bigint -> order*)
		val << : bigint * bigint -> bool
		val <<= : bigint * bigint -> bool
		val >> : bigint * bigint -> bool
		val >>= : bigint * bigint -> bool
		val == : bigint * bigint -> bool
		val len : bigint -> int
		(*val lenCompare : bigint * bigint -> order*)
		val lenLt : bigint * bigint -> bool
		val lenLeq : bigint * bigint -> bool
		val lenGt : bigint * bigint -> bool
		val lenGeq : bigint * bigint -> bool
		val lenEq : bigint * bigint -> bool
		val -- : bigint * bigint -> bigint
		val pred : bigint -> bigint
		(*exception division_by_zero*)
		(*val %% : bigint * bigint -> bigint * bigint
		val div : bigint * bigint -> bigint
		val mod : bigint * bigint -> bigint
		val quo : bigint * bigint -> bigint
		val rem : bigint * bigint -> bigint*)
		end
= 
struct
	(*int 1 tell no. is negative and 0 tells no. is positive*)
	type bigint = int * Bignat.bignat

	val bigzero=(0,[])

	fun normalize (b:bigint) =
		let 
			val norm  = Bignat.normalize(#2 b)
		in
			if(#1 b=0) then (#1 b,norm)
			else (#1 b,#2 b)
		end
		
	fun toString (b:bigint)=
		let 
			val str = Bignat.toString(#2 b)
		in 
			Int.toString(#1 b)^(str)
		end;

	fun ~~(b:bigint)=
		if(#1 b=0) then (1,#2 b)
		else (0,#2 b)

	fun abs(b:bigint)=
		if(#1 b=1) then (0,#2 b)
		else (0,#2 b)

	infix ++;
	fun (a:bigint) ++ (b:bigint)=
		if(#1 a=0 andalso #1 b=0) then (0,Bignat.++((#2 a),(#2 b)))
		else if(#1 a=0 andalso #1 b=1)  then 
			(
				if(Bignat.>>((#2 a),(#2 b))) then (0,Bignat.--((#2 a),(#2 b)))
				else (1,Bignat.--((#2 b),(#2 a)))
				)
		else if(#1 a=1 andalso #1 b=0)  then 
			(
				if(Bignat.>>((#2 a),(#2 b))) then (1,Bignat.--((#2 a),(#2 b)))
				else (0,Bignat.--((#2 b),(#2 a)))
				)
		else (1,Bignat.++((#2 a),(#2 b)))

	fun succ (0,[]) = (0,[1])
	|	succ (b:bigint) = b ++ (0,[1]);

	fun min(a:bigint,b:bigint)=
		if(#1 a=0 andalso #1 b=0) then (0,Bignat.min(#2 a,#2 b))
		else if(#1 a=1 andalso #1 b=1) then (1,Bignat.min(#2 a,#2 b))
		else if (#1 a=1 andalso #1 b=0) then a
  		else b

  	fun max(a:bigint,b:bigint)=
		if(#1 a=0 andalso #1 b=0) then (0,Bignat.max (#2 a,#2 b))
		else if(#1 a=1 andalso #1 b=1) then (1,Bignat.max (#2 a,#2 b))
		else if (#1 a=1 andalso #1 b=0) then b
  		else a

  	fun sign (a:bigint)=
  		if(#1 a=0) then 0
  		else 1

  	fun sameSign(a:bigint,b:bigint)=
  		if(#1 a = #1 b) then true
  		else false

  	infix **;
  	fun (a:bigint) ** (b:bigint)=
  		if(#1 a = #1 b) then (0,Bignat.** (#2 a,#2 b))
  		else (1, Bignat.** (#2 a,#2 b))

  	infix --;
  	fun (a:bigint) -- (b:bigint)=
		if(#1 a=0 andalso #1 b=0) then
		(	
		 	if(Bignat.>>(#2 a,#2 b)) then (0,Bignat.--((#2 a),(#2 b)))
		 	else (1,Bignat.--((#2 b),(#2 a)))
		)
		else if(#1 a=0 andalso #1 b=1)  then (0,Bignat.++((#2 a),(#2 b)))
		else if(#1 a=1 andalso #1 b=1)  then
			(
				if(Bignat.>>(#2 a,#2 b)) then (1,Bignat.--((#2 a),(#2 b)))
		 		else (0,Bignat.--((#2 a),(#2 b)))
			)
		else (1,Bignat.++((#2 a),(#2 b)))

  	fun pred (0,[]) = (1,[1])
	|	pred (b:bigint) = b -- (0,[1]);

	infix <<;
	fun (a:bigint) << (b:bigint)=
		if(min(a,b)=a) then true
		else false

	fun equal(a:bigint,b:bigint)=
		if(#1 a = #1 b) then(
			if(Bignat.==(#2 a,#2 b)) then true
			else false
			)
		else false


	infix <<=;
	fun (a:bigint) <<= (b:bigint)=
		if(min(a,b)=a) then true
		else if(equal(a,b)) then true
		else false

	infix >>;
	fun (a:bigint) >> (b:bigint)=
		if(min(a,b)=b) then true
		else false 

	infix >>=;
	fun (a:bigint) >>= (b:bigint)=
		if(min(a,b)=b) then true
		else if(equal(a,b)) then true
		else false 

	infix ==;
	fun (a:bigint) == (b:bigint)=
		if(equal(a,b)) then true
		else false 

	fun len(a:bigint)=length(#2 a)

	fun lenLt(a:bigint,b:bigint)=
		if(length(#2 a) < length(#2 b)) then true
		else false

	fun lenLeq(a:bigint,b:bigint)=
		if(length(#2 a) <= length(#2 b)) then true
		else false 

	fun lenGt(a:bigint,b:bigint)=
		if(length(#2 a) > length(#2 b)) then true
		else false 

	fun lenGeq(a:bigint,b:bigint)=
		if(length(#2 a) >= length(#2 b)) then true
		else false 

	fun lenEq(a:bigint,b:bigint)=
		if(length(#2 a) = length(#2 b)) then true
		else false 
end
structure Bigint = BigInt(Bignat);
infix ==;
infix >>=;
infix >>;
infix <<=;
infix <<;
infix --;
infix ++;

