-module('2018113001_2').

-export([main/1, proc/1, run/1]).

% Read the input data from the file

-record(graph, {source, n, m, edges, procs}).

readFile(FileName) ->
    {ok, Device} = file:open(FileName, [read]),
    [ValP] = readIntegers(io:get_line(Device, "")),
    [ValN, ValM] = readIntegers(io:get_line(Device, "")),
    Edges = readEdges(Device, ValM),
    [NodeS] = readIntegers(io:get_line(Device, "")),
    file:close(Device),
    #graph{source = NodeS, n = ValN, m = ValM,
	   edges = Edges, procs = ValP}.

readEdges(Device, ValM) ->
    if ValM =< 0 -> [];
       true ->
	   Edge = readIntegers(io:get_line(Device, "")),
	   [Edge] ++ readEdges(Device, ValM - 1)
    end.

readIntegers(Line) ->
    lists:map(fun (X) ->
		      {Int, _} = string:to_integer(X), Int
	      end,
	      string:tokens(Line, " ")).

% Do the BellMan Ford Loop

initializeDistances(ValN, NodeS) ->
    Distances = [initialDistanceValue(ID, NodeS)
		 || ID <- lists:seq(1, ValN)],
    Distances.

initialDistanceValue(Node, NodeS) ->
    if Node == NodeS -> 0;
       true -> 999999999
    end.

relaxValue(NewWeight, Position, Distances) ->
    OldWeight = lists:nth(Position, Distances),
    if NewWeight < OldWeight ->
	   lists:sublist(Distances, Position - 1) ++
	     [NewWeight] ++ lists:nthtail(Position, Distances);
       true -> Distances
    end.

relaxEdge(Edge, Distances) ->
    [U, V, W] = Edge,
    DistU = lists:nth(U, Distances),
    DistV = lists:nth(V, Distances),
    RelaxA = relaxValue(DistU + W, V, Distances),
    RelaxB = relaxValue(DistV + W, U, RelaxA),
    RelaxB.

relaxAllEdges(Edges, Distances) ->
    if Edges == [] -> Distances;
       true ->
	   [Head | Tail] = Edges,
	   NewDistances = relaxEdge(Head, Distances),
	   relaxAllEdges(Tail, NewDistances)
    end.

% Run the Algorithm

repeatRelax(Edges, Distances, N) ->
    RelaxedDistances = relaxAllEdges(Edges, Distances),
    if N > 1 -> repeatRelax(Edges, RelaxedDistances, N - 1);
       true -> RelaxedDistances
    end.

bellmanFord(Graph) ->
    ValN = Graph#graph.n,
    NodeS = Graph#graph.source,
    Edges = Graph#graph.edges,
    InitialDistances = initializeDistances(ValN, NodeS),
    Distances = repeatRelax(Edges, InitialDistances, ValN),
    Distances.

% Run the Show

writeFile(FileName, Distances, CurrentNodeID) ->
    {ok, Device} = file:open(FileName, [write]),
    writeLines(Device, Distances, CurrentNodeID),
    file:close(Device).

writeLines(Device, Distances, CurrentNodeID) ->
    if Distances == [] -> done;
       true ->
	   [Head | Tail] = Distances,
	   io:format(Device, "~p ~p\n", [CurrentNodeID, Head]),
	   writeLines(Device, Tail, CurrentNodeID + 1)
    end.

run(Args) ->
    [InputFile, OutputFile] = Args,
    Input = readFile(InputFile),
    Distances = runDistribution(Input),
    writeFile(OutputFile, Distances, 1).

% Distributed Bellman Ford Algorithm

relaxProcess(Process, Distances) ->
    Process ! {dist, Distances},
    receive {relaxed, NewDistances} -> NewDistances end,
    BestDistances = lists:zipwith(fun (X, Y) -> min(X, Y)
				  end,
				  Distances, NewDistances),
    BestDistances.

relaxAllProcesses(Processes, Distances) ->
    if Processes == [] -> Distances;
       true ->
	   [Head | Tail] = Processes,
	   NewDistances = relaxProcess(Head, Distances),
	   relaxAllProcesses(Tail, NewDistances)
    end.

repeatSolve(Processes, Distances, N) ->
    RelaxedDistances = relaxAllProcesses(Processes,
					 Distances),
    if N > 1 ->
	   repeatSolve(Processes, RelaxedDistances, N - 1);
       true -> RelaxedDistances
    end.

% Create and pass around edge list

distributeEdges([], _Edges) -> ok;
distributeEdges(_Processes, []) -> ok;
distributeEdges(Processes, Edges) ->
    if length(Edges) > 0 ->
	   SubSize = length(Edges) div length(Processes),
	   [ThisProcess | NextProcesses] = Processes,
	   {TheseEdges, NextEdges} = lists:split(SubSize, Edges),
	   ThisProcess ! {edges, TheseEdges},
	   distributeEdges(NextProcesses, NextEdges);
       true -> done
    end.

% Create and wait for nodes

proc(ParentID) ->
    receive
      {edges, EdgeList} ->
	  relaxationHandler(ParentID, EdgeList)
    end.

relaxationHandler(ParentID, Edges) ->
    receive
      {dist, Distances} ->
	  NewDistances = relaxAllEdges(Edges, Distances),
	  ParentID ! {relaxed, NewDistances},
	  relaxationHandler(ParentID, Edges)
    end.

% Maintain the server

runDistribution(Graph) ->
    NumNodes = Graph#graph.procs,
    Edges = Graph#graph.edges,
    Processes = [spawn(?MODULE, proc, [self()])
		 || _ID <- lists:seq(1, NumNodes)],
    distributeEdges(Processes, Edges),
    ValN = Graph#graph.n,
    NodeS = Graph#graph.source,
    InitialDistances = initializeDistances(ValN, NodeS),
    Distances = repeatSolve(Processes, InitialDistances,
			    ValN),
    Distances.

main(Args) ->
    [InputFile, OutputFile] = Args,
    Input = readFile(InputFile),
    Distances = runDistribution(Input),
    writeFile(OutputFile, Distances, 1).
