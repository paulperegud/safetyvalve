-module(sv_tracer).

-behaviour(gen_server).

-export([start_link/1, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, handle_info/2, code_change/3]).

-export([write_event/2]).

-record(state, { fd, tracer }).

start_link(Filename) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Filename], []).
    
stop() ->
    gen_server:call(?MODULE, stop).
    
write_event({trace, _Pid, call, {sv, report, [Now, {dodequeue, QSize, Sojourn}]}, _}, {Start, Fd}) ->
    file:write(Fd, [integer_to_list(Now - Start), $,, integer_to_list(QSize), $,, integer_to_list(Sojourn), $\n]),
    {Start, Fd}.

%% Callbacks
init([Filename]) ->
    {ok, Fd} = file:open(Filename, [write, binary, delayed_write]),
    file:write(Fd, header()),
    {TS, _} = sv:timestamp(),
    {ok, Tracer} = dbg:tracer(process, {fun write_event/2, {TS div 1000, Fd}}),
    dbg:p(all, [c]),
    dbg:tp(sv, report, 2, c),
    {ok, #state { fd = Fd,  tracer = Tracer}}.
	
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(Call, _From, State) ->
    {stop, {odd_call, Call}, State}.
    
handle_cast(Cast, State) ->
    {stop, {odd_cast, Cast}, State}.

handle_info(Info, State) ->
    {stop, {odd_info, Info}, State}.
    
terminate(_Reason, #state { fd = Fd }) ->
    file:close(Fd),
    dbg:stop_clear(),
    ok.

code_change(_OldVsn, State, _Aux) ->
    {ok, State}.

header() ->
    ["Time, QSize, Sojourn\n"].
