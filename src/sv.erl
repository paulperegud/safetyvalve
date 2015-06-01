-module(sv).

-export([timestamp/0, new/1, new/2, destroy/1, ask/2, done/3, reschedule/3]).
-export([run/2]).
%% Internal API
-export([report/2]).

%% @doc Creates a new queue
%% @end
-spec new(Conf) -> {ok, pid()}
    when
      Conf :: proplists:proplist().
new(Conf) ->
  new(undefined, Conf).

-spec new(Queue, Conf) -> {ok, pid()}
    when
      Queue :: undefined | atom(),
      Conf :: proplists:proplist().
new(Queue, Conf) ->
    {ok, Pid} = safetyvalve_sup:start_queue(Queue, Conf),
    {ok, Pid}.

%% @doc Destroys a previously created queue
%% @end
-spec destroy(Queue) -> ok | {error, not_found | simple_one_for_one}
    when
      Queue :: undefined | atom().
destroy(Queue) ->
    safetyvalve_sup:stop_queue(Queue).

%% @doc Enqueue a job on a queue
%% <p>Try to run `Fun' on queue `Name'. The `Fun' is run at time `TP'.
%% This means that either the
%% function will run straight away, or be queued for some time until
%% it is allowed to run (in case of an overload scenario). The
%% function will return either the result of `Fun' or an `{error,
%% Reason}' error term, describing the overload situation encountered.</p>
%% @end

-spec run(Name, Fun) -> {ok, Result} | {error, Reason}
    when
      Name :: atom() | pid(),
      Fun :: fun (() -> term),
      Result :: term(),
      Reason :: term().
run(Name, Fun) ->
    StartPoint = timestamp(),
    case sv_queue:ask(Name, StartPoint) of
        {go, Ref} ->
            Res = Fun(),
            EndPoint = timestamp(),
            sv_queue:done(Name, Ref, EndPoint),
            {ok, Res};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc ask/2 requests the use of a resource in safetyvalve
%% <p>Ask for the use of a `Queue' at timepoint `T'. Returns either `{go, Ref}' if
%% you are allowed to use the resource or `{error, Reason}' in case of an error</p>
%% <p>The timepoint `T' should be generated via a call to `sv:timestamp()'. Also, note
%% that this call will block until the resource is either given, or the system gives
%% up on processing the request because it has exceeded some queueing threshold.</p>
%% <p>When you are done processing, you are obliged to call `sv:done(Queue, Ref, TE)'
%% where `Ref' is the given reference and `TE' is a time endpoint as given by
%% a call to `sv:timestamp()'.
%% @end
-spec ask(Queue, T) -> {go, Ref} | {error, Reason}
  when
    Queue :: atom() | pid(),
    T :: any(),
    Ref :: term(), % Opaque
    Reason :: term().
ask(QN, T) ->
  sv_queue:ask(QN, T).

%% @doc done/3 relinquishes a resource yet again to the queue
%% <p>Call this function when you are done with using a resource. @see ask/2 for the
%% documentation of how to invoke this function.</p>
-spec done(Queue, Ref, TE) -> ok
  when
    Queue :: atom(),
    Ref :: term(),
    TE :: any().
done(QN, R, TE) ->
  sv_queue:done(QN, R, TE).

%% @doc reschedule/3 allows to change priority of task
%% <p>You may use this function to change order of tasks in particular queue. 
%% @see ask/2 for the documentation of how to invoke this function.</p>
reschedule(QN, T1, T2) ->
    sv_queue:reschedule(QN, T1, T2).

%% @private
report(_T, _Event) ->
    hopefully_traced.

%% @doc Construct a timestamp in a canonical way for Safetyvalve.
%% The important rule here is that timestamps are used as unique time
%% representations, which in turn means we have to create a timestamp
%% and latch on a unique integer.
%% @end
-spec timestamp() -> {integer(), integer()}.
timestamp() ->
	T = sv_time:monotonic_time(micro_seconds),
	U = sv_time:unique_integer(),
	{T, U}.
