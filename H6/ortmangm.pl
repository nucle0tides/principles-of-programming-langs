%% Gabby Ortman, Homework 6

 :-[airlines].

%% Q1

%% appending empty list to any list 
append([], L, L).

%% appending non-empty list
append([X|L1], L2, [X|L]) :-
    append(L1, L2, L).

%% In class notes
member(X, [X|_]) :- !.

member(X, [_|L]) :-     
	member(X, L).

member(X, [L|_]) :- 
	member(X, L).

%% Modified base rule Reach from in class notes 
travel(Origin, Destination, Visited, Price, Duration, Stops, Route) :-
	not(member(Destination, Visited)),  
	flight(Airline, Origin, Destination, FlightCost, FlightTime), 
	airport(Origin, Tax, Delay), 
	Price is FlightCost + Tax, 
	Duration is FlightTime + Delay,
	Stops is 1, 
	append([[Origin, Airline, Destination]], [], R), 
	Route = R.


%% Modified recursive Reach from in class notes
travel(S, T, Visited, Price, Duration, Stops, Route) :-
	flight(Airline, S, T1, FlightCost, FlightTime),
	not(member(T1, Visited)),	
	travel(T1, T, [T1|Visited], P1, D1, S1, R1),
	Price is FlightCost + P1, 
	Duration is FlightTime + D1,
	Stops is S1 + 1, 
	append([[S, Airline, T1]], R1, R), 
	Route = R.

trip(Origin, Destination, Route) :-
	travel(Origin, Destination, [], P1, D1, N1, R1), 
	Origin = Origin, 
	Destination = Destination, 
	Price = P1, 
	Duration = D1,
	NumAirlines = N1, 
	append([Price, Duration, NumAirlines, R1], [], R), 
	Route = R.

trip_m(Origin, Destination, [Price, Duration, NumAirlines, Route]) :-
	travel(Origin, Destination, [], P1, D1, N1, R1), 
	Origin = Origin, 
	Destination = Destination, 
	Price = P1, 
	Duration = D1,
	NumAirlines = N1, 
	Route = R1. 


%% Q2 
tripk(Origin, Destination, K, Route) :- 
	trip_m(Origin, Destination, [P1, D1, N1, R1]), 
	D1 =< K, 
	Origin = Origin, 
	Destination = Destination, 
	Price = P1, 
	Duration = D1,
	NumAirlines = N1, 
	append([Price, Duration, NumAirlines, R1], [], R), 
	Route = R.

tripk_m(Origin, Destination, K, [Price, Duration, NumAirlines, Route]) :- 
	trip_m(Origin, Destination, [P1, D1, N1, R1]), 
	D1 =< K, 
	Origin = Origin, 
	Destination = Destination, 
	Price = P1, 
	Duration = D1,
	NumAirlines = N1, 
	Route = R1.

%% Q3 
multicitytrip(Origin, Destination, Intermediate, Route) :-
	travel(Origin, Destination, [], P1, D1, N1, R1),
	Origin = Origin, 
	Destination = Destination, 
	Price = P1, 
	Duration = D1,
	NumAirlines = N1, 
	append([Price, Duration, NumAirlines, R1], [], R), 
	Route = R,
	member(Intermediate, R1).

multicitytrip_m(Origin, Destination, Intermediate, [Price, Duration, NumAirlines, Route]) :-
	travel(Origin, Destination, [], P1, D1, N1, R1),
	Origin = Origin, 
	Destination = Destination, 
	Price = P1, 
	Duration = D1,
	NumAirlines = N1, 
	Route = R1,
	member(Intermediate, R1).


%% EXTRA CREDIT              

findbesttrips(Origin, Destination, Visited, Route) :-
	not(member(Destination, Visited)),  
	flight(Airline, Origin, Destination, FlightCost, FlightTime),
	airport(Origin, Tax, _),  
	Cost is FlightCost + Tax,
	append([Origin, Destination, Cost], [], R), 
	Route = R.

findbesttrips(S, T, Visited, Route) :-
	flight(Airline, S, T1, FlightCost, FlightTime),
	not(member(T1, Visited)),	
	findbesttrips(T1, T, [T1|Visited], R1),  
	Cost is FlightCost + P1,
	append([S, T1, Cost], R1, R), 
	Route = R.

findbesttrips(Origin, R) :-
	findall(R, findbesttrips(Origin, _, [], R), Final), 
	%% here I would take the minimum cost out but alas i am tired
	R = Final.