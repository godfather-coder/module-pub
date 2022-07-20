# ocaml-module
    module Rationals : RationalField =
      struct
        type t = int * int
        exception Bad_rational of string
        let zero = ...
        ...
        let standard_form (n,d) = ...
        (* A rational in the standard form has its numerator and denominator
           relatively prime and the denominator is positive 
         *)

        let compare (r1,i1) (r2,i2) = ...
        (* Compare two rational numbers as the Ocaml compare function
           would compare two pairs. Make sure that the rationals are brought  
           in the standard form before comparision
         *)
         ...
        (* Define add, mul, sub, div operations for rationals,
           define also their additive and multiplicative inverses, 
           raise exeption Bad_rational if denominator becomes zero.

           Define to_float function that gives the decimal expansion of 
           a rational number. Define also from_int function that makes
           a rational out of an integer.
         *)    
      end

In the module GaussianRationals the structure can be as follows:

    module GaussianRationals : GaussianRationalField =
      struct
        type t = (int * int) * (int * int)
        exception Division_by_zero of string
        let zero = ...
        ...
        let compare (r1,i1) (r2,i2) = ...
        (* for compare the rationals r1 and r2. If Rationals.compare
           for them does not give 0, then retiurn it. Otherwise
           return the result of Rationals.compare for i1 and i2 
         *)

        let to_string (r,i) = ...
        (* make sure that both the real part r and the imaginary part i
           are in standard form as a rational number. 
           Skip a summand if it is 0. 
         *)

        let from_rational r =
          (* Make a Gaussaan rational from a rational number by putting
             the rational as the real part of the Gaussian rational.
           *)
          ...
        (* Define add, mul, sub, div operations for Gaussian rationals,
           define also their additive and multiplicative inverses, raise
           the expeption Division_by_zero if necessary.

           Define the function re (for the real part of a Gaussian
           rational, im (for the imaginary part) and conj (for the conjugate) 
         *)    
       end

The operations add, mul, sub, div correspond respectively to addition, multiplication, subtraction and division in the corresponding field. For module Rationals they are defined as the corresponding operations for rational numbers. Make sure that the results of all operations are returned in the strandard form, which means that the number should be transformed so that the numerator and the denominator are relatively prime, and the denominator is always positive.

Gaussian rationals are complex numbers with rational coefficients.
Hence, add, mul, sub, and div are addition, multiplication, subtraction and division operations are those for complex numbers, where the elementary operations are perfomed over rationals (for this, you can use the operations defined in the module Rationals).

In to_string function, return a string that correspond to the standard form of a rational number and, besides, for numbers with the denominator 1 return only the string form of the numerator; for number with the numerator 0 return 0; Skip a summand if it is 0.

WRITE YOUR IMPLEMENTATION BELOW, AFTER THE DEFINITIONS OF THE SIGNATURES.

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

module type RationalField =
  sig
    include Field with type t = int * int
    type t = int * int          (* rationals are represented as pairs of int *)
    exception Bad_rational of string
    val standard_form : t -> t  (* standard from of a rational number *)
    val to_float : t -> float   (* decimal expansion *)
    val from_int : int -> t     (* integer to rational conversion *)          
  end

module type GaussianRationalField =
  sig
    include Field with type t = (int * int) * (int * int)
    (* Gaussian rationals are represented as pairs of rationals *)
    exception Division_by_zero of string
    val from_rational : (int * int ) -> t   (* rational to complex *)     
    val conj : t -> t                       (* conjugate *)
    val re : t -> (int * int)               (* real part *)
    val im : t -> (int * int)               (* imaginary part *)
  end
