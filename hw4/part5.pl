%-----------
%---PART5---
%-----------
process(L,LT,RT) :-
   split(L,LL,RL),              
   term(LL,LT),                 
   term(RL,RT),                 
   LT =:= RT.                   


term([X],X).                    
term(L,T) :-                    
   split(L,LL,RL),              
   term(LL,LT),                 
   term(RL,RT),                 
   operation_(LT,RT,T).            
operation_(LT,RT,LT+RT).
operation_(LT,RT,LT-RT).
operation_(LT,RT,LT*RT).
operation_(LT,RT,LT/RT) :- RT =\= 0.   


split(L,L1,L2) :- append(L1,L2,L), L1 = [_|_], L2 = [_|_].


solve(L) :- 
   process(L,LT,RT),
      writef('%w = %w\n',[LT,RT]),
      
   fail.
solve(_).
%tests
%[7,91,13,7,21]
%[6,91,13,7,20]
%[6,91,13,20,33]