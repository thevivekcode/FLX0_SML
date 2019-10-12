use "signatureFLX.sml";

structure Flx : FLX =
struct
      
exception Not_wellformed
exception Not_nf
exception Not_int

datatype term = VAR of string (* variable *)
	  | Z           (* zero *)
	  | T           (* true *)
	  | F           (* false *)
	  | P of term   (* Predecessor *)
	  | S of term   (* Successor *)
	  | ITE of term * term * term   (* If then else *)
	  | IZ of term  (* is zero *)
	  | GTZ of term (* is greater than zero *)
	  
local
fun	checkSPZ(term) = let 
			in
			(case term of
			Z => true
			| S term' => checkSPZ(term')
			| P term' => checkSPZ(term')
			| _ => false)
			end
			
fun	toInt_algo(Z , value) = value

|	toInt_algo(S(S(input)) , value) = toInt_algo(S(input) , value + 1)
|	toInt_algo(S(P(input)) , value) = if(checkSPZ input) then raise Not_nf (*NOT IN NORAML FROM EXCP*)
					else raise Not_int (* NOT INTEGER EXCP*)
|	toInt_algo(S(Z) , value) = toInt_algo(Z , value + 1)

|	toInt_algo(P(P(input)) , value) = toInt_algo(P(input) , value - 1)
|	toInt_algo(P(S(input)) , value) = if(checkSPZ input) then raise Not_nf (*NOT IN NORAML FROM EXCP*)
					else raise Not_int (* NOT INTEGER EXCP*)
|	toInt_algo(P(Z) , value) = toInt_algo(Z , value - 1)

|	toInt_algo(_,_)= raise Not_int (* NOT INTEGER EXCP*)
					
in (*LOCAL IN*)
fun toInt(input) = toInt_algo(input , 0);
end (* LOCAL END*)

local
fun	fromInt_algo(0,output) = output
|	fromInt_algo(value, output) = if value > 0 then fromInt_algo(value -1 ,S(output))
				 	else fromInt_algo(value+1 ,P(output))

in (*LOCAL IN*)				 	
fun 	fromInt(value) = fromInt_algo(value , Z)
end (* LOCAL END*)	

local
fun	checkSPZ(term) = let 
			in
			(case term of
			Z => true
			| S term' => checkSPZ(term')
			| P term' => checkSPZ(term')
			| _ => false)
			end

in
fun	normalize((VAR X)) = (VAR X)
|	normalize(Z) = Z
|	normalize(T) = T
|	normalize(F) = F
|	normalize (P term) = let
				val nfp = normalize term
				in 
				(case nfp of
				Z => (P Z)
				| S x => x
				| P x => (P nfp)
				| _ => (P (normalize nfp))
				)
				end
|	normalize (S term) = let
				val nfp = normalize term
				in 
				(case nfp of
				Z => (S Z)
				| S x => (S nfp)
				| P x =>  x
				| _ => (S (normalize nfp))
				)
				end
|	normalize(IZ(term)) = 	let
				val nfp = normalize term
				in
				if (checkSPZ(nfp)) then (case nfp of
				Z => T
				|_ => F)
				else IZ(nfp)
				end
|	normalize(GTZ(term)) = 	let
				val nfp = normalize term
				in 
				if (checkSPZ(nfp)) then (case nfp of
				S nfp' => T
				| _ => F)
				else GTZ(nfp)
				end				
|	normalize(ITE(term1,term2,term3))= let
						val t1 = normalize(term1)
						val t2 = normalize(term2)
						val t3 = normalize(term3)
						in
						if t1 = T then t2
						else if t1=F then t3
						else if t2=t3 then t2
						else ITE(t1,t2,t3)
						end
						
end (*LOCAL ENDS*)
						
fun	toString(T) = "T"
|	toString(F) = "F"
|	toString(VAR X)= X
|	toString(Z) = "Z"
|	toString(P(term)) = "("^"P"^" "^toString(term)^")"
|	toString(S(term)) = "("^"S"^" "^toString(term)^")"
|	toString(IZ(term)) = "("^"IZ"^" "^toString(term)^")"
|	toString(GTZ(term)) = "("^"GTZ"^" "^toString(term)^")"
|	toString(ITE(term1 , term2 , term3 ))= 
	"("^"ITE"^" "^"<"^toString(term1)^","^toString(term2)^","^toString(term3)^">"^")"
	
	
(*
(ITE <(ITE <(ITE<T,F,T>),F,(S (P Z))>),(S (S Z)),T>)  
*)

local 
fun	dividebycomma([],_,_,_,_)= raise Not_wellformed
|	dividebycomma(charlist,_,2,list1,list2)=(implode(rev(list1)),implode(rev(list2)),implode(charlist))
|	dividebycomma(charlist as h::t , Bcount,Ccount, list1,list2)=  
     if (h = #"(") andalso (Ccount=0) then dividebycomma(t , Bcount+1,Ccount, h::list1 ,list2)
else if (h = #")") andalso (Ccount=0) then dividebycomma(t , Bcount-1,Ccount, h::list1 ,list2)
else if (h = #"(") andalso (Ccount=1) then dividebycomma(t , Bcount+1,Ccount, list1 ,h::list2)
else if (h = #")") andalso (Ccount=1) then dividebycomma(t , Bcount-1,Ccount, list1 ,h::list2)
else if (Bcount =0) andalso (h = #",")  then dividebycomma(t , Bcount, Ccount+1, list1,list2 )
else if (Ccount=0) then dividebycomma(t , Bcount, Ccount, h::list1,list2 )
else if (Ccount=1) then dividebycomma(t , Bcount, Ccount, list1,h::list2 )
else raise Not_wellformed


fun 	isVar([])= true
|	isVar(inp as h::t) = if (h >= #"a") andalso (h <= #"z") then isVar(t)
				else false

fun	frmStr(inp)= 
		 if (inp = "T") then T
		else if (inp = "F") then F
		else if (inp = "Z") then Z
		else if (String.isPrefix "(P " inp) andalso (String.isSuffix ")" inp) then P(frmStr(substring(inp,3,(size(inp)-4))))
		else if (String.isPrefix "(S " inp) andalso (String.isSuffix ")" inp) then S(frmStr(substring(inp,3,(size(inp)-4))))
		else if (String.isPrefix "(IZ " inp) andalso (String.isSuffix ")" inp) then IZ(frmStr(substring(inp,4,(size(inp)-5))))
		else if (String.isPrefix "(GTZ " inp) andalso (String.isSuffix ")" inp) then GTZ(frmStr(substring(inp,5,(size(inp)-6))))
		else if (String.isPrefix "(ITE <" inp) andalso (String.isSuffix ">)" inp) then 
						let
						val inp' = (substring(inp,6,(size(inp)-8)))
						val (t1,t2,t3)= dividebycomma(explode(inp'),0,0,[],[])
						in
						ITE(frmStr(t1),frmStr(t2),frmStr(t3))
						end
						
		else if (String.isPrefix " " inp) then frmStr(substring(inp,1,(size(inp)-1)))
		else if (inp="") then raise Not_wellformed
		else if (isVar(explode(inp))) then VAR inp
		else if (String.isPrefix "(" inp) andalso not(String.isSuffix ")" inp) then raise Not_wellformed
                else raise Not_wellformed
		
in (*LOCAL IN*)
fun fromString(input )= frmStr(input)

end(*LOCAL ENDS*)


end (*STRUCT END*)	




(*	

frmStr "(ITE <(ITE <(IZ Z),(S Z),(P Z)>),(S Z),(P Z)>)";

 ITE ((ITE ((IZ Z),(S Z),(P Z))),(S Z),(P Z));

*)		


		
			
              
			    

		




				




		

			
