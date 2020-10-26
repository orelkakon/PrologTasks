%Code by Orel Kakon.
%%%---------------------------------------------------------------------------------------PART 1--------------------------------------------------------------%%%

% nat :- N is a natural number
  nat(0).
  nat(s(X)) :- nat(X).


% add(X,Y,Z) :-
%    X,Y,Z are natural numbers 
%    such that  Z = X+Y
  add(0, X, X) :- nat(X).
  add(s(X), Y, s(Z)) :- add(X,Y,Z).


% leq(X,Y) :-
%   X,Y are natural numbers 
%    such that X is less or equal than Y
  leq(0,_).
  leq(s(X),s(Y)) :- leq(X,Y).


% lt(X,Y) :-
%   X,Y are natural numbers 
%    such that X is less than Y
  lt(0,s(_)).
  lt(s(X),s(Y)) :- lt(X,Y).


% times(X,Y,Z) :-
%    X,Y,Z are natural numbers 
%    such that  Z = X*Y
  times(0,X,0) :- nat(X).
  times(s(X),Y,Z) :- lt(X,Z), leq(Y,Z) , times(X,Y,W), add(W,Y,Z).


% TASK_1: unary_sqrt(N,K) :- 
%     K is the square-root of N,
%     where N and K, are unary numbers. 
unary_sqrt(0,0).
unary_sqrt(N,K) :- times(K,K,N) , lt(0,N).

%%%% Its work for me in mode of "unary_sqrt(-,+)" and mode of "unary_sqrt(+,-) because its used in predicate "times" (my times - that Im wrote) that work on this mode" 

% TASK_1: unary_sqrt_rev(N,K) :- 
%     K is the square-root of N,
%     where N and K, are unary numbers. 
unary_sqrt_rev(0,0).
unary_sqrt_rev(N,K) :- times(K,K,N) , lt(0,N).


% TASK_2: unary_divisor(N,K) :-
%     For integers n and k, k is a divisor of n 
%     if there exists r such that n = k · r
unary_divisor(N,K) :- lt(0,N), times(K,R,N), nat(R).

%%%% Its work for me in mode of "unary_divisor(-,+)" and mode of "unary_divisor(+,-) because its used in predicate "times" (my times - that Im wrote) that work on this mode" 

% TASK_2: unary_divisor_rev(N,K) :-
%     For integers n and k, k is a divisor of n 
%     if there exists r such that n = k · r
unary_divisor_rev(N,K) :- lt(0,N), times(K,R,N), nat(R).

%%%---------------------------------------------------------------------PART 2-------------------------------------------------------------------------------%%%

% append(X,Y,Z)
%     Z is XUY
  append([],L,L).
  append([H|T],L2,[H|L3]) :- append(T,L2,L3).


% rev_list(X,Y)
%     Y is List so return the reverse of X.
  rev_list([],[]).
  rev_list([X|Xs],Y) :- rev_list(Xs,Ys) , append(Ys,[X],Y).
  
  
% first_member(X).
%     X is 1	  
  first_member(1).


% is_member(X).
%     return true iff X is 1 or 0
  is_member(0).
  is_member(1).

  
% binaryList(X)
%     X is simple binary list (include just 1's and 0's).
  binaryList([]).
  binaryList([X | Xs]) :- nonvar(X), is_member(X), binaryList(Xs).

  
% TASK_3: is_binary(BinList) :-
%      succeeds if and only if B is a legal natural number in binary notation
  is_binary(BinList) :- nonvar(BinList) , is_binaryWrap(BinList).
  
  
% is_binaryWrap(BinList) :-
%      (WRAP FUNCTION) succeeds if and only if B is a legal natural number in binary notation
  is_binaryWrap(B) :- rev_list(B,[X|Xs]), nonvar(X), first_member(X), binaryList(Xs).   

  
% TASK_4: binary_plus(X,Y,Z) :-
%       succeeds if and only if X and Y and Z are natural numbers in    
%       binary notation such that X+Y=Z.
  binary_plus([], [1|Yr], Z) :- append([], [1|Yr], Z).
  binary_plus([], [0|Yr], Z) :- append([], [0|Yr], Z).
  binary_plus([0|Xr], [], Z) :- append([0|Xr], [], Z).
  binary_plus([1|Xr], [], Z) :- append([1|Xr], [], Z).
  binary_plus([],[],[]).
  binary_plus([1|Xr],[0|Yr], Z) :- binary_plus(Xr,Yr,Z1), append([1],Z1, Z).
  binary_plus([0|Xr],[1|Yr], Z) :- binary_plus(Xr,Yr,Z1), append([1],Z1, Z).
  binary_plus([0|Xr],[0|Yr], Z) :- binary_plus(Xr,Yr,Z1), append([0],Z1, Z).
  binary_plus([1|Xr],[1|Yr], Z) :- binary_plus(Xr,Yr,Z1), binary_plus([1],Z1, Z2), append([0],Z2, Z).  

  
% binary_plus(-,-,+) dont success in this implemention because "binary_plus" predicate dont support this. 
	
	
% TASK_4: binary_plus_rev(X,Y,Z) 
  binary_plus_rev(X,Y,Z) :- binary_plus_wrap(X,Y,Z), is_binaryWithotNonVar(X), is_binaryWithotNonVar(Y).
  binary_plus_wrap([], [1|Yr], Z) :- append([], [1|Yr], Z).
  binary_plus_wrap([], [0|Yr], Z) :- append([], [0|Yr], Z).
  binary_plus_wrap([0|Xr], [], Z) :- append([0|Xr], [], Z).
  binary_plus_wrap([1|Xr], [], Z) :- append([1|Xr], [], Z).
  binary_plus_wrap([],[],[]).
  binary_plus_wrap([1|Xr],[0|Yr], [0|Zr]) :- append([1],Z1, [0|Zr]), binary_plus_wrap(Xr,Yr,Z1).
  binary_plus_wrap([1|Xr],[0|Yr], [1|Zr]) :- append([1],Z1, [1|Zr]), binary_plus_wrap(Xr,Yr,Z1).
  binary_plus_wrap([0|Xr],[1|Yr], [0|Zr]) :- append([1],Z1, [0|Zr]), binary_plus_wrap(Xr,Yr,Z1).
  binary_plus_wrap([0|Xr],[1|Yr], [1|Zr]) :- append([1],Z1, [1|Zr]), binary_plus_wrap(Xr,Yr,Z1).
  binary_plus_wrap([1|Xr],[1|Yr], [0|Zr]) :- append([0],Z2, [0|Zr]), binary_plus_wrap([1],Z1, Z2), binary_plus_wrap(Xr,Yr,Z1).  
  binary_plus_wrap([1|Xr],[1|Yr], [1|Zr]) :- append([0],Z2, [1|Zr]), binary_plus_wrap([1],Z1, Z2), binary_plus_wrap(Xr,Yr,Z1).  
  binary_plus_wrap([0|Xr],[0|Yr], [0|Zr]) :- append([0],Z1, [0|Zr]), binary_plus_wrap(Xr,Yr,Z1).
  binary_plus_wrap([0|Xr],[0|Yr], [1|Zr]) :- append([0],Z1, [1|Zr]), binary_plus_wrap(Xr,Yr,Z1).
  
% same(X,Y) :- X is Y
  same(1,1).
  same(0,0).
  
  
% equal_list(L1,L2) :- L1 is L2 excatly.
  equal_list([],[]).
  equal_list([X|Xrest],[Y|Yrest]) :- same(X,Y), equal_list(Xrest,Yrest).
  
  
% is_binaryWithotNonVar(BinList) :-
%      (WRAP FUNCTION) copy just for this question.
  binaryListWithotNonVar([]).
  binaryListWithotNonVar([X | Xs]) :- is_member(X), binaryListWithotNonVar(Xs).  
  is_binaryWithotNonVar([]).
  is_binaryWithotNonVar(B) :- rev_list(B,[X|Xs]), first_member(X), binaryListWithotNonVar(Xs). 

  
% TASK_5: binary_palindrome(X) :-
%		takes a list of variables, and unifies it with a number in binary representation 
%		such that the bits of B form a palindrome.
  binary_palindrome(X) :- is_binaryWithotNonVar(X), rev_list(X,Y), equal_list(X,Y).
      
  
% TASK_6: binary_palindrome_sum(N, A, B, C) :-
%		which unifies A, B and C, with three binary palindromes such that 
%		the binary addition A+B+C=N and 0=<A=<B=<C
  binary_palindrome_sum(N,A,B,C) :- binary_plus_rev(C,_,N), binary_plus_rev(B,_,C), binary_plus_rev(A,_,B),
						binary_palindrome(C), binary_palindrome(B), binary_palindrome(A), binary_plus(A,B,AB), binary_plus(AB,C,N).
  										
										
%%%---------------------------------------------------------------------PART 3-------------------------------------------------------------------------------%%%
											

% TASK_7: is_prime(X) :- 
%		which succeeds if and only if
%		X is a prime number.																	
  is_prime(X) :-
    X > 2, 
    1 is mod(X,2),
	Y is sqrt(X),
    N_Limit is floor(Y), 
    forall(between(2, N_Limit, Divider), mod(X, Divider) > 0). 
  is_prime(2).  
 
% TASK_8: right_prime(N)
%		A prime number n is right truncatable if 
%  		every prefix of n is also a prime number  
  right_prime(N) :- is_prime(N), N1 is N//10, right_prime2(N1).
  right_prime2(N) :- is_prime(N), N1 is N//10, right_prime2(N1).
  right_prime2(0).

% right_prime(-) dont success in this implemention because "is_prime" predicate dont support this. 
  
% increase(Num,Var) :- Var is increase evrey round from the number Num by one. 
  increase(Num, Num).
  increase(Num, Var) :- NewNum is Num + 1, increase(NewNum, Var).
  
  
% TASK_8: right_prime_gen(X) :- generates right truncatable primes upon calling.
  right_prime_gen(X) :- increase(2,X), right_prime(X).