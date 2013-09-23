%
%Assignment : 3
%Author 	: Sahil Kumar
%Date		: 18/9/2013
%
%
%Run Code:
%	Test Case 1:
%		program(value,0,5,5,C0,C1,LS).
%	Test Case 2:
%		program(value,5,15,15,C0,C1,LS).
%	Test Case 3:
%		program(square,0,5,5,C0,C1,LS).
%	Test Case 4:
%		program(cubeplusone,0,5,5,C0,C1,LS).
%	Test Case 5:
%		program(plusone,0,5,5,C0,C1,LS).


value(X,A):- A is X.
square(X,A):- A is X*X.
cubeplusone(X,A):- A is X*X*X + 1.
plusone(X,A):- A is X+1.

%FN = function
%A :is the starting point
%H : Interval between two points
%N : Total Number of Intervals
%SUM : SUM
%NOW : The current number
%Call by passing NOW =0

%Sigma X
sum_x(A,H,N,NOW,SUM):- N=:=NOW, SUM is A+NOW*H.
sum_x(A,H,N,NOW,SUM):- N>0, sum_x(A,H,N,NOW+1,SS), SUM is SS + A+NOW*H.

%Sigmna X^2
sum_xx(A,H,N,NOW,SUM):- N=:=NOW, SUM is (A+NOW*H)*(A+NOW*H).
sum_xx(A,H,N,NOW,SUM):- N>0, sum_xx(A,H,N,NOW+1,SS), SUM is SS + (A+NOW*H)*(A+NOW*H).

%Sigma F(x)
sum_fx(FN,A,H,N,NOW,SUM):- N=:=NOW, call(FN,A+NOW*H,SS) , SUM is SS.
sum_fx(FN,A,H,N,NOW,SUM):- N>0, call(FN,A+NOW*H,TT) ,sum_fx(FN,A,H,N,NOW+1,SS), SUM is SS + TT.

%Sigma x*F(x)
sum_xfx(FN,A,H,N,NOW,SUM):- N=:=NOW, call(FN,A+NOW*H,SS) , SUM is (A+NOW*H)*SS.
sum_xfx(FN,A,H,N,NOW,SUM):- N>0, call(FN,A+NOW*H,TT) ,sum_xfx(FN,A,H,N,NOW+1,SS), SUM is SS + (A+NOW*H)*TT.

%LSE
lse(FN,A,H,N,NOW,SUM,C0,C1):- N=:=NOW, call(FN,A+NOW*H,SS) , SUM is (SS-(C0+C1*(A+NOW*H)))*(SS-(C0+C1*(A+NOW*H))).
lse(FN,A,H,N,NOW,SUM,C0,C1):- N>0, call(FN,A+NOW*H,TT),lse(FN,A,H,N,NOW+1,SS,C0,C1), SUM is SS + (TT-(C0+C1*(A+NOW*H)))*(TT-(C0+C1*(A+NOW*H))).

%Cramer Rule
cramer(A,B,E,C,D,F,X,Y):-A*D-B*C =\=0, X is (E*D-B*F)/(A*D-B*C), Y is (A*F-E*C)/(A*D-B*C).

%Program
program(FN,A,B,N,C0,C1,LS):- 
A1E is N+1,H is (B-A)/N ,
call(sum_x,A,H,N,0,B1E),
call(sum_fx,FN,A,H,N,0,E1E), 
C1E is B1E,
call(sum_xx,A,H,N,0,D1E),
call(sum_xfx,FN,A,H,N,0,F1E),
call(cramer,A1E,B1E,E1E,C1E,D1E,F1E,C0,C1),
call(lse,FN,A,H,N,0,LS,C0,C1).