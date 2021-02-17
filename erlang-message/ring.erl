-module(ring).

-export([connect/1, create/1, node/2]).

-define(MAXVAL, 100000).

create(NumNodes) when NumNodes > 1 ->
    %creates the ring's nodes
    Nodes = [spawn(?MODULE, node, [ID, self()])
	     || ID <- lists:seq(1, NumNodes)],
    % connects them in a ring
    ring:connect(Nodes),
    % sends the token in the ring
    hd(Nodes) ! {token, 3}.

%connects the nodes to a ring
connect(N = [H | _]) -> connect_(N ++ [H]).

connect_([]) -> connected;
connect_([_]) -> connected;
connect_([N1, N2 | Nodes]) ->
    N1 ! {self(), connect, N2}, connect_([N2 | Nodes]).

%the node function. Initially waits for the next node's pid
node(ID, MasterPID) ->
    receive
      {MasterPID, connect, NextPID} ->
	  node(ID, MasterPID, NextPID)
    end.

node(ID, MasterPID, NextPID) ->
    receive
      {token, Val} ->
	  io:format("[~p:~p] token value ~p~n",
		    [ID, self(), Val]),
	  NextPID ! {token, Val},
	  MasterPID ! {self(), exit}
    end.
