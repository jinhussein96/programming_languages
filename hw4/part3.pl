%-----------
%---PART3---
%-----------




when(102,10).
when(108,12).
when(341,14).
when(455,16).
when(452,17).

where(102,z23).
where(108,z11).
where(341,z06).
where(455,207).
where(452,207).



enroll(a,102).
enroll(a,108).
enroll(b,102).
enroll(c,108).
enroll(d,341).
enroll(e,455).

schedule(S, P, T) :- enroll(S, C),
					 where(C, P), 
					 when(C,T).

usage(P,T) :-	where(X,P),        
				when(X,T). 

conflict(X,Y):- (when(X,A),
				 when(Y,B), 
				 A==B)
				; %or
				(where(X,C), 
					where(Y,D),
					C==D).
meet(X,Y) :- enroll(X,C1),
			enroll(Y,C2), 
			when(C1,T1) , 
			when(C2,T2) ,
			where(C1,P1) , 
			where(C2,P2) , 
			abs(T1-T2) < 2, =(P1,P2).