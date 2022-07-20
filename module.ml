module type Field = sig
  type t
  val zero : t                  (* zero element of the field *)
  val one : t                   (* unit element of the field *)
  val compare : t -> t -> int   (* comparison *)
  val to_string : t -> string   (* field element to string *)
  val add : t -> t -> t         (* addition *)
  val mul : t -> t -> t         (* multiplication *)
  val sub : t -> t -> t         (* subtraction *)
  val div : t -> t -> t         (* division *)
  val add_inv : t -> t          (* additive inverse *) 
  val mul_inv : t -> t          (* multiplicative inverse *)
end
module type RationalField   =
sig
  include Field with type t = int * int
  type t = int * int          (* rationals are represented as pairs of int *)
  exception Bad_rational of string
  val standard_form : t -> t  (* standard from of a rational number *)
  val to_float : t -> float   (* decimal expansion *)
  val from_int : int -> t     (* integer to rational conversion *)          
end
module Rationals: RationalField  =
struct
  type t = int * int
  exception Bad_rational of string
  let zero = (0,1)
  let one = (1,1)
  let standard_form (n,d) = let rec gcd a b = if b = 0 then a else gcd b (a mod b) in let denom = gcd n d in (n/denom,d/denom)
  let compare (r1,i1) (r2,i2) =
    let (a,b)= standard_form(r1,i1) in 
    let (a2,b2) = standard_form (r2,i2) in 
    let  comp1 = a*b2 in 
    let comp2  = b*a2 in 
    if comp1 > comp2 then 1 else
    if comp1 < comp2 then -1 else 0
  let mul (a,b) (c,d) = if (b*d) <> 0 then  standard_form( a*c,b*d ) else  raise  (Bad_rational"Bad_rationals") 
  let  sub (a,b) (c,d) = if (b*d) <> 0 then standard_form(((a*d) - (b*c)),(b*d)) else  raise  (Bad_rational"Bad_rationals") 
  let div (a,b) (c,d) = if (b*c)<>0 then standard_form(a*d,b*c) else  raise  (Bad_rational"Bad_rationals") 
  let add (a,b) (c,d) =if (b*d) <> 0 then  standard_form(((a*d) +(b*c)),(b*d))else  raise  (Bad_rational"Bad_rationals") 
  let to_float(n,d) = if d <> 0 then (float)n/.(float)d else raise  (Bad_rational"Bad_rationals")
  let from_int b = (b,1)
  let mul_inv (n,d)= if n<>0 then (d,n) else raise(Bad_rational"Bad_rationals_inverse")
  let add_inv(n,d)= (-n,d)
  let to_string (n,d)= "("^ string_of_int (n) ^ "," ^ string_of_int (d)^")"
end

module type GaussianRationalField =
sig
  include Field with type t = (int * int) * (int * int)
     
  exception Division_by_zero of string
  val from_rational : (int * int ) -> t   (* rational to complex *)     
  val conj : t -> t                       (* conjugate *)
  val re : t -> (int * int)               (* real part *)
  val im : t -> (int * int)               (* imaginary par*)
end

module GaussianRationals :GaussianRationalField  =
struct
  type t = (int * int) * (int * int)
  exception Division_by_zero of string
  let zero = ((0,1),(0,1))
  let one=((1,1),(0,1))
  let compare (r1,i1) (r2,i2) = if Rationals.compare r1 r2 <> 0 then Rationals.compare r1 r2 else Rationals.compare  i1 i2
      
  let to_string (r,i) = "("^(Rationals.to_string r)^"," ^(Rationals.to_string i) ^ ")"
       
  let from_rational r = (r,(0,1))
         
  let sub (r,i) (r1,i1) = ( (Rationals.sub r r1), Rationals.sub i i1 )
  let add (r,i) (r1,i1)=if (Rationals.add r r1) = Rationals.zero || (Rationals.add i i1) = Rationals.zero then raise(Division_by_zero"") else
      ((Rationals.add r r1),Rationals.add i i1) 
      
  let mul (r,i) (r1,i1) = let firsts = Rationals.mul r r1 in 
    let outers = Rationals.mul r i1 in 
    let inners = Rationals.mul i r1 in
    let lasts = Rationals.mul i i1 in
    let helperu = Rationals.mul(-1,1) lasts in
    let real = Rationals.add firsts helper in 
    let imaginary = Rationals.add outers inners in
    (real, imaginary)
    
  let  div (a,b) (c,d) =let ac = Rationals.mul a c in
    let bd = Rationals.mul b d in
    let bc = Rationals.mul b c in
    let ad = Rationals.mul a d in
    let c2 = Rationals.mul c c in 
    let d2 = Rationals.mul d d in 
    let acbd = Rationals.add ac bd in
    let c2d2 = Rationals.add c2 d2 in
    let bc_ad = Rationals.sub bc ad in
    let real_part = Rationals.div acbd c2d2 in
    let imaginary_part = Rationals.div bc_ad c2d2 in
    if c2 = (0,1) && d2 = (0,1) then raise (Division_by_zero"oh nooo!") 
    else (Rationals.standard_form real_part,Rationals.standard_form imaginary_part)
    
  let re (r,i)= r
  let im (r,i)= i
  let conj(r,i)=(r, Rationals.mul (-1,1) i)
  let add_inv(r,i)=let both(d,c)=(-d,c) in (both(r),both(i))
  let mul_inv (r,i)=
    if r = (0,1)&& i = (0,1) then raise(Division_by_zero "") else
      let real = Rationals.div (r) (Rationals.add (Rationals.mul r r) (Rationals.mul i i))in
      let imaginary = Rationals.add_inv (Rationals.div (i) (Rationals.add (Rationals.mul r r) (Rationals.mul i i)))in
      (real,imaginary)
end;;
