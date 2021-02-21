-module('2018113001_1').

-export([main/1, node/2]).

-define(MAXVAL, 100000).

readIntegers(Line) ->
    lists:map(fun (X) ->
		      {Int, _} = string:to_integer(X), Int
	      end,
	      string:tokens(Line, " ")).

readFile(FileName) ->
    {ok, Device} = file:open(FileName, [read]),
    [ValP, ValM] = readIntegers(io:get_line(Device, "")),
    file:close(Device),
    [ValP, ValM].

create(NumNodes) when NumNodes > 1 ->
    Nodes = [spawn(?MODULE, node, [ID, self()])
	     || ID <- lists:seq(1, NumNodes)],
    connect(Nodes),
    hd(Nodes).

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
      {token, Val, SenderID, OutputFile} ->
	  io:format("~p got a token ~p from sender ~p~n",
		    [ID, Val, SenderID]),
	  {ok, Device} = file:open(OutputFile, [append]),
	  io:format(Device, "~p got a token ~p from sender ~p~n",
		    [ID, Val, SenderID]),
	  file:close(Device),
	  io:format("~p released~n", [ID]),
	  NextPID ! {token, Val, ID, OutputFile},
	  MasterPID ! {self(), exit}
    end.

main(Args) ->
    [InputFile, OutputFile] = Args,
    [ValP, ValM] = readFile(InputFile),
    RootNode = create(ValP),
    RootNode ! {token, ValM, ValP, OutputFile}.
