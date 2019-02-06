% knowledge base
flight(istanbul,izmir,3).
flight(izmir,antalya,1).
flight(antalya,diyarbakir,5).
flight(antalya,erzurum,2).
flight(erzurum,edirne,5).
flight(izmir,ankara,6).
flight(istanbul,ankara,2).
flight(istanbul,trabzon,3).
flight(ankara,trabzon,6).
flight(ankara,kars,3).
flight(kars,gaziantep,3).
flight(ankara,diyarbakir,8).

% rules
/**
% route(source,destination,cost) will show the path from source to destination and total cost for this path
% route(source,X,C) will show the all path you can go from source and total cost
*/

route(Start,End,Cost) :- searchGraph(Start,End,[Start],Cost,0).

/** 
% route will use the searchGraph rule to search the graph.
% base rule for search graph that it reaches the end
*/
searchGraph(Start,End,Path,Cost,Acc) :-  ((nonvar(End), Start == End); var(End)) , Cost = Acc, reverse(Path,NewPath), format("Path ="), writeln(NewPath).

/**
% recursive rule for search graph, it will go to the next connected location
*/
searchGraph(Start,End,Path,Cost,Acc) :- connectedFlight(Start,Next,X), not(member(Next,Path)), NewCost is Acc+X, searchGraph(Next,End,[Next|Path],Cost,NewCost).

/* 
% connectedFlight(source,destination,cost) will show the path that is only connected directly
*/
connectedFlight(Source,Destination,Cost) :- flight(Source,Destination,Cost).
connectedFlight(Source,Destination,Cost) :- flight(Destination,Source,Cost).




