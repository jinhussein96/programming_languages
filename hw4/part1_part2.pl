%-----------
%---PART1---
%-----------

% knowledge base

flight(istanbul,rize).
flight(rize,istanbul).

flight(istanbul,izmir).
flight(izmir,istanbul).


flight(istanbul,ankara).
flight(ankara,istanbul).


flight(istanbul,van).
flight(van,istanbul).


flight(istanbul,gaziantep).
flight(gaziantep,istanbul).


flight(istanbul,antalya).
flight(antalya,istanbul).


flight(izmir,isparta).
flight(isparta,izmir).


flight(isparta,burdur).
flight(burdur,isparta).


flight(edirne,edremit).
flight(edremit,edirne).


flight(edremit,erzincan).
flight(erzincan,edremit).


flight(antalya,konya).
flight(konya,antalya).


flight(antalya,gaziantep).
flight(gaziantep,antalya).


flight(konya,ankara).
flight(ankara,konya).


flight(ankara,van).
flight(van,ankara).

flight(van,rize).
flight(rize,van).

distance(rize,van,373.01).
distance(van,rize,373.01).


distance(istanbul, rize, 967.79).
distance(rize,istanbul, 967.79).


distance(izmir,istanbul, 328.80).
distance(istanbul,izmir, 328.80).


distance(istanbul, ankara , 351.50).
distance( ankara ,istanbul, 351.50).


distance( van ,istanbul, 1262.37).
distance( istanbul,van ,1262.37).


distance( istanbul,gaziantep,847.42).
distance( gaziantep,istanbul,847.42).


distance(istanbul,antalya, 482).
distance(antalya,istanbul, 482).


distance( isparta,izmir,308.55).
distance( izmir,isparta,308.55).


distance(burdur,isparta,24.60).
distance(isparta,burdur,24.60).


distance(edirne,edremit,833.87).
distance(edremit,edirne,833.87).


distance(edremit,erzincan,736.34).
distance(erzincan,edremit,736.34).


distance(konya,antalya,192.28).
distance(antalya,konya,192.28).


distance(antalya,gaziantep,592.33).
distance(gaziantep,antalya,592.33).


distance(ankara,konya,227.34).
distance(konya,ankara,227.34).


distance(van,ankara,920.31).
distance(ankara,van,920.31).



% rules 

route(X,Y):- flight(X,Y),
			 flight(Y,X).
route(X,Y):-flight(X,A),
            flight(B,Y),
            X\=Y,
            (flight(A,B);A==B) .






%-----------
%---PART2---
%-----------


sroute(C1,C2,X) :- flight(C1,C2),
				   distance(C1,C2,X).
sroute(C1,C2,X) :- route(C1,C3), 
				   route(C3,C2),
				   distance(C1,C3,X1),
				   distance(C3,C2,X2), 
				   X is X1+X2.


