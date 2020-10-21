-module(test_rps).
-import(rps,[start/0, queue_up/3, move/2, drain/3]). 
-export([test_all/0, playerspawn/3]).

%% Maybe you want to use eunit
-include_lib("eunit/include/eunit.hrl").


%% DONT FORGET TO MOVE TO TESTS FOLDER BEFORE SUBMITTING %%

test_all() ->
    {ok, Broker} = rps:start(),
    eunit:test(
      [
       start_broker(),
       drain_test_empty(),
       join_game(Broker),
       play_round_lose(Broker),
       play_round_win(Broker),
       play_round_tie(Broker),
       play_win_game(Broker),
       play_lose_game(Broker),
       error_nonpositive(Broker),
       statistics_test(Broker),
       error_join_after_drain(Broker) 
      ], [verbose]).


start_broker() ->
    {"Start a broker, and nothing else",
     fun() ->
             ?assertMatch({ok, _}, rps:start())
     end}.

drain_test_empty() ->
    {"Start a broker, drain it, get message",
     fun() ->
            {ok, S} = rps:start(),
            rps:drain(S, self(), "testmsg"),
            receive
                Msg -> ?assertEqual("testmsg", Msg)
            end
     end}.

join_game(Broker) ->
  {"Start a paperbot, join a game with it",
    fun() ->
    %   {ok, S} = rps:start(),
      spawn(test_rps, playerspawn, [Broker, 'BOT', 5]),
      ?assertMatch({ok, 'BOT', _}, rps:queue_up(Broker, 'human', 5))
    %   io:write("~p", [S])
    end}.

play_round_lose(Broker) ->
    {"Start a paperbot, join a game with it, lose once",
    fun() ->
    %   {ok, S} = rps:start(),
      spawn(test_rps, playerspawn, [Broker, 'BOT1', 5]),
      {ok, 'BOT1', CoordID} = rps:queue_up(Broker, 'human1', 5),
      ?assertMatch(round_lost, rps:move(CoordID, rock))
    %   io:write("~p", [S])
    end}.

play_round_win(Broker) ->
    {"Start a paperbot, join a game with it, win once",
    fun() ->
    %   {ok, S} = rps:start(),
      spawn(test_rps, playerspawn, [Broker, 'bot2', 5]),
      {ok, 'bot2', CoordID} = rps:queue_up(Broker, 'human2', 5),
      ?assertMatch(round_won, rps:move(CoordID, scissors))
    %   io:write("~p", [S])
    end}.

play_round_tie(Broker) ->
    {"Start a paperbot, join a game with it, tie once",
    fun() ->
    %   {ok, S} = rps:start(),
      spawn(test_rps, playerspawn, [Broker, 'bot3', 5]),
      {ok, 'bot3', CoordID} = rps:queue_up(Broker, 'human3', 5),
      ?assertMatch(tie, rps:move(CoordID, paper))
    %   io:write("~p", [S])
    end}.

play_win_game(Broker) ->
    {"Start a paperbot, join a game with it, lose",
    fun() ->
    %   {ok, S} = rps:start(),
      spawn(test_rps, playerspawn, [Broker, 'bot4', 5]),
      {ok, 'bot4', CoordID} = rps:queue_up(Broker, 'human4', 5),
      ?assertMatch(round_won, rps:move(CoordID, scissors)),
      ?assertMatch(round_won, rps:move(CoordID, scissors)),
      ?assertMatch({game_over, 3, 0}, rps:move(CoordID, scissors))
    %   io:write("~p", [S])
    end}.

play_lose_game(Broker) ->
    {"Start a paperbot, join a game with it, win",
    fun() ->
    %   {ok, S} = rps:start(),
      spawn(test_rps, playerspawn, [Broker, 'bot5', 5]),
      {ok, 'bot5', CoordID} = rps:queue_up(Broker, 'human5', 5),
      ?assertMatch(round_lost, rps:move(CoordID, rock)),
      ?assertMatch(round_lost, rps:move(CoordID, rock)),
      ?assertMatch({game_over, 0, 3}, rps:move(CoordID, rock))
    %   io:write("~p", [S])
    end}.

statistics_test(Broker) ->
    {"Start three new bots, check statistics, drain, check stats again",
    fun() ->
        ?assertMatch({ok, 3, 0, 4}, rps:statistics(Broker)),
        spawn(test_rps, playerspawn, [Broker, 'bot6', 5]),
        spawn(test_rps, playerspawn, [Broker, 'bot7', 15]),
        spawn(test_rps, playerspawn, [Broker, 'bot8', 135]),
        timer:sleep(15),
        ?assertMatch({ok, 3, 3, 4}, rps:statistics(Broker)),
        rps:drain(Broker, self(), "Drainmsg"),
        ?assertMatch({ok, 3, 0, 4}, rps:statistics(Broker))
        % {ok, 'bot5', CoordID} = rps:queue_up(Broker, 'human5', 5),
        % ?assertMatch({round_lost}, rps:move(CoordID, rock)),
        % ?assertMatch({round_lost}, rps:move(CoordID, rock)),
        % ?assertMatch({game_over, 0, 3}, rps:move(CoordID, rock))
    %   io:write("~p", [S])
    end}.

error_nonpositive(Broker) ->
    {"Try to queue up with negative numbers",
    fun() ->
    %   {ok, S} = rps:start(),
        Response = rps:queue_up(Broker, 'cant_count', -4),
        ?assertEqual({error, nonpositive_rounds}, Response)
    %   spawn(test_rps, playerspawn, [Broker, 'bot5', 5]),
    %   {ok, 'bot5', CoordID} = rps:queue_up(Broker, 'human5', 5),
    %   ?assertMatch(round_lost, rps:move(CoordID, rock)),
    %   ?assertMatch(round_lost, rps:move(CoordID, rock)),
    %   ?assertMatch({game_over, 0, 3}, rps:move(CoordID, rock))
    %   io:write("~p", [S])
    end}.

error_join_after_drain(Broker) ->
    {"Try to queue up to a broker that is draining",
    fun() ->
        Response = rps:queue_up(Broker, 'WantsToPlay', 5),
        ?assertEqual(server_stopping, Response)
    end}.

% Functions for spawning bots


playPaperLoop(CoordID) ->
    % io:fwrite("Bot making a move~n"),
    case rps:move(CoordID, paper) of
        {game_over, _, _} -> 
            io:fwrite("(bot) Game ended...~n");
        % {server_stopping} ->
        %     io:fwrite("Server stopping (bot)"),
        %     ControllerID ! {draining};
        server_stopping -> server_stopping;
        Something -> 
            io:fwrite("Bot got back: ~p~n", [Something]),
            % timer:sleep(1000),
            playPaperLoop(CoordID)
    end.

playerspawn(BrokerRef, Name, Rounds) -> 
    register(Name, self()),
    % io:fwrite("Bot started."),
    case rps:queue_up(BrokerRef, Name, Rounds) of
        {ok, _, Coordinator} -> playPaperLoop(Coordinator);
        {error, {error}} -> error;
        server_stopping -> server_stopping;
        Anything -> Anything
    % io:fwrite("(~p) Got reply: ~p~n", [Name, Response]),
    end.


