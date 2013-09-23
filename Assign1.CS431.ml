(*
 * Assignment : 1
 * Author     : Sahil Kumar
 * Roll_No    : 10010175 
 *)

(*Function Declarations*)
fun square(x:real)= x*x;
fun cubeplus1(x:real)=x*x*x+1.0;

(*
 * Returns the Coefficient that must be multiplied
 *)
fun getCoefficient(n:int, i:int)=
  if i=0 orelse i=2*n then
    1.0
  else
    if i mod 2 = 0 then
      2.0
    else
      4.0;
(*
 * Function does the actual computation
 *)
fun get_Simpson(a:real, b:real, n:int, F, i:int, h:real)=
  if i < 0 then 
    (real)0
  else if i>=0 andalso i<=2*n then
    getCoefficient(n,i) *F(a+(real)i*h) + get_Simpson(a,b,n,F,i-1,h)
  else
    (real)0;

(*
 * Function that needs to be called
 *)
fun simpson(a:real, b:real, n:int, F)=
  let 
    exception BadN;
    val h = (b-a) / ( (real)(2*n))
  in
    if n < 0 then raise BadN
    else
      (h/3.0)*get_Simpson(a,b,n,F,2*n,h)
  end;
