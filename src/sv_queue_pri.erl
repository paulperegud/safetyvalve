%%% @doc Extension of sv_queue_ets with additional interface which allows to reinsert items
%%% @end
-module(sv_queue_pri).

-export([new/0, delete/1]).

-export([out/2, len/1, in/3]).

-export([re/3]).

-export([remove/3]).

remove(Item, TS, QName) ->
    true = ets:delete_object(QName, {TS, Item}),
    QName.

new() ->
	ets:new(queue, [protected, ordered_set]).
	
delete(Q) -> ets:delete(Q).

out(_Ts, QName) ->
	case ets:first(QName) of
		'$end_of_table' -> {empty, [], QName};
		Key ->
			[{_T, E} = Obj] = ets:lookup(QName, Key),
			true = ets:delete_object(QName, Obj),
			{E, [], QName}
	end.
	
len(QName) ->
	ets:info(QName, size).
	
%% Format is kept like this to make sure it follows that of the `queue' module.
in(Item, Ts, QName) ->
	true = ets:insert_new(QName, {Ts, Item}),
	QName.

%% reinsert Item from Ts1 to Ts2
re(Ts1, Ts2, QName) ->
    case ets:lookup(QName, Ts1) of
	[{_T, Term}] ->
	    true = ets:delete(QName, Ts1),
	    true = ets:insert_new(QName, {Ts2, Term});
	[] ->
	    noop
    end,
    QName.
