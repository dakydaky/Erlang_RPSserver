-module(rps).
-behaviour(gen_server).
-export([start/0, queue_up/3, move/2, statistics/1, drain/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, coordLoop/7]).

start() -> gen_server:start_link(?MODULE, [], []).


coordEvalRound(BrokerRef, P1ID, P2ID, P1Score, P2Score, MaxRounds, PlayedRounds, MoveWinner) ->
    case gen_server:call(BrokerRef, are_we_draining) of
        yes -> 
            % io:fwrite("Coord dying.~n"),
            P1ID ! server_stopping, 
            P2ID ! server_stopping,
            gen_server:cast(BrokerRef, {done_draining, {P1ID, P2ID, self()}});
        no ->
            if %all the cases where we're not draining
                P1Score >= (MaxRounds/2) -> 
                    % io:fwrite("P1 has won the game"),
                            P1ID ! {game_over, P1Score, P2Score},
                            P2ID ! {game_over, P2Score, P1Score},
                            gen_server:cast(BrokerRef, {game_ended, PlayedRounds, {P1ID, P2ID, self()}});
                            %check if the MaxRounds is > LongestGame, NewLongestGame
                P2Score >= (MaxRounds/2) -> 
                    % io:fwrite("P2 has won the game"),
                            P1ID ! {game_over, P1Score, P2Score},
                            P2ID ! {game_over, P2Score, P1Score},
                            gen_server:cast(BrokerRef, {game_ended, PlayedRounds, {P1ID, P2ID, self()}});
                            %check if the MaxRounds is > LongestGame, NewLongestGame
                true ->
                    case MoveWinner of
                        none -> 
                            P1ID ! tie,
                            P2ID ! tie,
                            % io:fwrite("Round was a tie!"),
                            coordLoop(BrokerRef, P1ID, P2ID, P1Score, P2Score, MaxRounds, PlayedRounds);
                        p1 ->
                            P1ID ! round_won,
                            P2ID ! round_lost,
                            % io:fwrite("P1 won the round!"),
                            coordLoop(BrokerRef, P1ID, P2ID, P1Score, P2Score, MaxRounds, PlayedRounds);
                        p2 ->
                            P1ID ! round_lost,
                            P2ID ! round_won,
                            % io:fwrite("P2 won the round!"),
                            coordLoop(BrokerRef, P1ID, P2ID, P1Score, P2Score, MaxRounds, PlayedRounds)
                    end
                end
    end.

coordEvalMoves(BrokerRef, P1ID, P2ID, P1Score, P2Score, MaxRounds, PlayedRounds, P1Move, P2Move) ->
    % io:fwrite("Entering coordEvalMoves~n"),
    Validmoves = [rock, scissors, paper],
    ValidP1 = lists:member(P1Move, Validmoves),
    ValidP2 = lists:member(P2Move, Validmoves),
    if  
        ValidP1 == false ->
            % io:fwrite("P2 wins hand (P1 foul) ~n"),
            coordEvalRound(BrokerRef, P1ID, P2ID, P1Score, P2Score+1, MaxRounds, PlayedRounds, p2);
        ValidP2 == false ->
            % io:fwrite("P1 wins hand (P2 foul) ~n"),
            coordEvalRound(BrokerRef, P1ID, P2ID, P1Score+1, P2Score, MaxRounds, PlayedRounds, p2);
        true ->
            case {P1Move, P2Move} of
                {Same, Same} -> 
                    coordEvalRound(BrokerRef, P1ID, P2ID, P1Score, P2Score, MaxRounds, PlayedRounds, none);
                {rock, scissors} -> 
                    % io:fwrite("P1 wins hand ~n"),
                    coordEvalRound(BrokerRef, P1ID, P2ID, P1Score+1, P2Score, MaxRounds, PlayedRounds, p1);
                {scissors, paper} -> 
                    % io:fwrite("P1 wins hand ~n"),
                    coordEvalRound(BrokerRef, P1ID, P2ID, P1Score+1, P2Score, MaxRounds, PlayedRounds, p1);
                {paper, rock} -> 
                    % io:fwrite("P1 wins hand ~n"), 
                    coordEvalRound(BrokerRef, P1ID, P2ID, P1Score+1, P2Score, MaxRounds, PlayedRounds, p1);
                {_, _} -> 
                    % io:fwrite("P2 wins hand ~n"),
                    coordEvalRound(BrokerRef, P1ID, P2ID, P1Score, P2Score+1, MaxRounds, PlayedRounds, p2)
            end
    end.

coordLoop(BrokerRef, P1ID, P2ID, P1Score, P2Score, MaxRounds, PlayedRounds) ->
    receive
        {P1ID, {move, P1Move}} -> P1M = P1Move
    end,
    % io:fwrite("Player 2 make a move: "),
    receive
        {P2ID, {move, P2Move}} -> P2M = P2Move
    end,
    coordEvalMoves(BrokerRef, P1ID, P2ID, P1Score, P2Score, MaxRounds, PlayedRounds+1, P1M, P2M).



init([]) ->
    State = {0, [], [], false, {"nothing", noone}},
    Return = {ok, State},
    %io:format("init: ~p~n", [State]),
    Return.

% State = {LongestGame, InQueue, Ongoing, Draining, {DrainMsg, DrainID}}
% InQueue = [{Name, ID, Rounds}...]
% Ongoing = [{FromID, OppID, CoordID}...],

handle_call(are_we_draining, _From, {LongestGame, InQueue, Ongoing, Draining, {DrainMsg, DrainID}}) ->
    % io:format("Broker replying to draining query~n"),
    if 
        Draining -> Return = {reply, yes, {LongestGame, InQueue, Ongoing, Draining, {DrainMsg, DrainID}}};
        true -> Return = {reply, no, {LongestGame, InQueue, Ongoing, Draining, {DrainMsg, DrainID}}}
    end,
    % io:format("handle_call: ~p~n", [Return]),
    Return;

handle_call({statistics}, _From, {LongestGame, InQueue, Ongoing, Draining, {DrainMsg, DrainID}}) ->
    % io:format("Fetching statistics, "),
    Return = {reply, {ok, LongestGame, erlang:length(InQueue), erlang:length(Ongoing)}, {LongestGame, InQueue, Ongoing, Draining, {DrainMsg, DrainID}}},
    Return;

handle_call(_Request, _From, State) ->
    % io:format("General case ~p~n", [_Request]),
    Reply = ok,
    Return = {reply, Reply, State},
    % io:format("handle_call: ~p~n", [Return]),
    Return.

handle_cast({done_draining, {P1ID, P2ID, From}}, {LongestGame, InQueue, Ongoing, Draining, {DrainMsg, DrainID}}) ->
    % io:format("Coordinator telling broker it is done draining~n"),
    NewOngoing = lists:delete({P1ID, P2ID, From}, Ongoing),
    % io:fwrite("Removed game from list, new list is: ~p~n", [NewOngoing]),
    if 
        erlang:length(NewOngoing) > 0 -> 
            Return = {noreply, {LongestGame, InQueue, NewOngoing, Draining}},
            Return;
        true -> 
            DrainID ! DrainMsg,
            gen_server:stop(self())
    end;

handle_cast({drain, PID, Msg}, {LongestGame, InQueue, Ongoing, _, {_, _}}) ->
    lists:map(fun({_, ID, _}) -> ID ! server_stopping end, InQueue),
    if
        erlang:length(Ongoing) > 0 ->
            Return = {noreply, {LongestGame, [], Ongoing, true, {Msg, PID}}},
            Return;
        true -> 
            PID ! Msg,
            gen_server:stop(self())
    end;

handle_cast({game_ended, GameLen, {P1ID, P2ID, GameID}}, {LongestGame, InQueue, Ongoing, Draining, {DrainMsg, DrainID}}) ->
    if
        GameLen > LongestGame -> 
            % io:format("New longest game recorded"),
            TmpLongest = GameLen;
        true -> TmpLongest = LongestGame
    end,
    NewOngoing = lists:delete({P1ID, P2ID, GameID}, Ongoing),
    % io:fwrite("Removed the game from the ongoing list, new list: ~p~n", [NewOngoing]),
    Return = {noreply, {TmpLongest, InQueue, NewOngoing, Draining, {DrainMsg, DrainID}}},
    Return;

handle_cast({queue_up, _, _, FromID}, {LongestGame, InQueue, Ongoing, true, {DrainMsg, DrainID}}) ->
    FromID ! server_stopping,
    Return = {noreply, {LongestGame, InQueue, Ongoing, true, {DrainMsg, DrainID}}},
    Return;

handle_cast({queue_up, Name, Rounds, FromID}, {LongestGame, InQueue, Ongoing, Draining, {DrainMsg, DrainID}}) ->
    % case Draining of
    %     true -> 
    %         FromID ! server_stopping,
    %         Return = {noreply, {LongestGame, InQueue, Ongoing, Draining}};
    %     false ->
    if 
        Rounds < 1 -> 
            FromID ! {error, nonpositive_rounds},
            {noreply, {LongestGame, InQueue, Ongoing, Draining, {DrainMsg, DrainID}}};
        true ->
            case lists:keysearch(Rounds, 3, InQueue) of
                {value, {OppName, OppID, _}} -> 
                    % io:fwrite("Found an opponent with name: ~p~n", [OppName]),
                    CoordID = spawn(rps, coordLoop, [self(), FromID, OppID, 0, 0, Rounds, 0]),
                    OppID ! {ok, Name, CoordID},
                    FromID ! {ok, OppName, CoordID},
                    % io:fwrite("Removing ~p~n", [{OppName, OppID, Rounds}]),
                    NewQueue = lists:delete({OppName, OppID, Rounds}, InQueue),
                    NewOngoing = [{FromID, OppID, CoordID} | Ongoing],
                    Return = {noreply, {LongestGame, NewQueue, NewOngoing, Draining, {DrainMsg, DrainID}}};
                    % io:fwrite("Match made, current queue is: ~p~n", [NewQueue]);
                false ->
                    NewQueue = [{Name, FromID, Rounds} | InQueue],
                    Return = {noreply, {LongestGame, NewQueue, Ongoing, Draining, {DrainMsg, DrainID}}}
                    % io:fwrite("Person added to queue, current queue is: ~p~n", [NewQueue])
            end,
            % end,
            Return
    end;

handle_cast(_Msg, State) ->
    Return = {noreply, State},
    % io:format("handle_cast: ~p~n", [Return]),
    Return.

handle_info(_Info, State) ->
    Return = {noreply, State},
    % io:format("handle_info: ~p~n", [Return]),
    Return.

terminate(_Reason, _State) ->
    % Return = ok,
    % io:format("terminate: ~p~n", [Return]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    Return = {ok, State},
    % io:format("code_change: ~p~n", [Return]),
    Return.


queue_up(BrokerRef, Name, Rounds) -> 
    gen_server:cast(BrokerRef, {queue_up, Name, Rounds, self()}),
    % io:fwrite("(~p) Cast, got reply: ~p~n", [Name, Reply]),
    receive
        {ok, OppName, Coordinator} -> {ok, OppName, Coordinator};
        server_stopping -> server_stopping;
        %{error, server_drain} -> server_stopping
        {error, Error} -> {error, Error}
    end.
    % io:fwrite("(~p)~p~n", [Name, Reply]).

move(Coordinator, Choice) -> 
    % io:fwrite("Imma make a move.~n"),
    Coordinator ! {self(), {move, Choice}},
    receive
        Reply -> Reply
    end.

statistics(BrokerRef) -> 
    Reply = gen_server:call(BrokerRef, {statistics}),
    Reply.
    % receive
    %     Reply -> Reply
    % end.

drain(BrokerRef, Pid, Msg) -> gen_server:cast(BrokerRef, {drain, Pid, Msg}).
