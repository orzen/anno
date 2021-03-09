-module(anno_associations).
-behavior(gen_server).

-ifdef(TEST).
-compile(export_all).
-endif.

-export([
    associate_sync/2,
    disassociate_sync/2,
    assocs_sync/1,   % list
    labels_sync/1,   % list
    lookup_assocs_sync/2,
    lookup_labels_sync/2,
    update_sync/2,
    start_link/1,
    stop/1
]).

-export([
    init/1,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

% IMPORTANT
% If registry is finding a suitable node. The search algo should first perform
% a lookup in the label table and secondly do a lookup in the node table. If
% the node exists in the node table then the registry can be sure that the node
% is not in the process of being added.

-include_lib("kernel/include/logger.hrl").
-include("gen_server_log.hrl").
-include("types.hrl").

-define(SERVER(X), {global, X}).


-record(state, {assocs :: #{atom() => sets:set()},
                labels :: #{atom() => sets:set()}}).

%% API %%

-spec associate_sync(Server :: atom(), Assocs :: [#assoc{}]) -> ok | {error, Reasons :: [string()]}.
associate_sync(Server, Assocs) when is_list(Assocs) ->
    gen_server:call(?SERVER(Server), {associate, Assocs}).

-spec disassociate_sync(Server :: atom(), Assocs :: [atom()]) -> ok | {error, Reasons :: [string()]}.
disassociate_sync(Server, Assocs) when is_list(Assocs) ->
    gen_server:call(?SERVER(Server), {disassociate, Assocs}).

assocs_sync(Server) ->
    gen_server:call(?SERVER(Server), {list, assocs}).

labels_sync(Server) ->
    gen_server:call(?SERVER(Server), {list, labels}).

lookup_assocs_sync(Server, Lookup_assocs) ->
    gen_server:call(?SERVER(Server), {lookup, assocs, Lookup_assocs}).

lookup_labels_sync(Server, Lookup_labels) ->
    gen_server:call(?SERVER(Server), {lookup, labels, Lookup_labels}).

-spec update_sync(Server :: atom(), Assocs :: [#assoc{}]) -> ok.
update_sync(Server, Assocs) ->
    gen_server:call(?SERVER(Server), {update, Assocs}).

start_link(Server) ->
    gen_server:start_link(?SERVER(Server), ?MODULE, [], []).

stop(Server) ->
    gen_server:stop(?SERVER(Server)).

%% CALLBACK %%

init(_Args) ->
    ?GS_INIT([]),
    {ok, #state{}, hibernate}.

terminate(Reason, _State) ->
    ?GS_TERM(Reason),
    ok.


% TODO change {L, A , F}

% CALL
handle_call({associate, Associate_assocs}, _From, #state{labels = Labels, assocs = Assocs} = State) ->
    case add_assocs(Associate_assocs, Labels, Assocs) of
        {Labels2, Assocs2, []} ->
            {reply, ok, State#state{labels = Labels2, assocs = Assocs2}, hibernate};
        {Labels2, Assocs2, Failed} ->
            {reply, {error, Failed}, State#state{labels = Labels2, assocs = Assocs2}, hibernate}
    end;
handle_call({disassociate, Disassociate_assocs}, _From, #state{labels = Labels, assocs = Assocs} = State) ->
    case remove_assocs(Disassociate_assocs, Labels, Assocs) of
        {Labels2, Assocs2, []} ->
            {reply, ok, State#state{labels = Labels2, assocs = Assocs2}, hibernate};
        {Labels2, Assocs2, Failed} ->
            {reply, {error, Failed}, State#state{labels = Labels2, assocs = Assocs2}, hibernate}
    end;
handle_call({update, Update_assocs}, _From,  #state{labels = Labels, assocs = Assocs} = State) ->
    case update_assocs(Update_assocs, Labels, Assocs) of
        {Labels2, Assocs2, []} ->
            {reply, ok, State#state{labels = Labels2, assocs = Assocs2}, hibernate};
        {Labels2, Assocs2, Failed} ->
            {reply, {error, Failed}, State#state{labels = Labels2, assocs = Assocs2}, hibernate}
    end;
handle_call({list, all}, _From, #state{labels = Labels, assocs = Assocs} = State) ->
    {reply, {ok, Labels, Assocs}, State, hibernate};
handle_call({list, labels}, _From, #state{labels = Labels} = State) ->
    {reply, {ok, Labels}, State, hibernate};
handle_call({list, assocs}, _From, #state{assocs = Assocs} = State) ->
    {reply, {ok, Assocs}, State, hibernate};
handle_call({lookup, assocs, Lookup_assocs}, _From, #state{assocs = Assocs} = State) ->
    Lookup_assocs = lookup(assocs, Lookup_assocs, Assocs),
    {reply, {ok, Lookup_assocs}, State, hibernate};
handle_call({lookup, labels, Lookup_labels}, _From, #state{labels = Labels} = State) ->
    Lookup_labels = lookup(labels, Lookup_labels, Labels),
    {reply, {ok, Lookup_labels}, State, hibernate};
handle_call(Req, From, State) ->
    ?GS_UNHANDLED_CALL(Req, From),
    {noreply, State, hibernate}.

% CAST
handle_cast(Req, State) ->
    ?GS_UNHANDLED_CAST(Req),
    {noreply, State, hibernate}.

% INFO
handle_info(Msg, State) ->
    ?GS_UNHANDLED_INFO(Msg),
    {noreply, State, hibernate}.

%% INTERNAL %%


% ADD
-spec add_to_labels(Assoc_name :: atom(), Label :: atom(), Labels :: #{atom() => list(atom())}) ->
    #{atom() => list(atom())}.
add_to_labels(Assoc_name, Label, Labels) ->
    case maps:find(Label, Labels) of
        % Add to existing set
        {ok, Label_assocs} ->
            S = sets:add_element(Assoc_name, Label_assocs),
            Labels#{Label => S};
        % Create new set
        error ->
            S = sets:add_element(Assoc_name, sets:new()),
            Labels#{Label => S}
    end.

-spec add_to_assocs(Assoc :: #assoc{}, Labels :: map(), Assocs :: map()) ->
    {Labels2 :: map(), Assocs :: map()}.
add_to_assocs(#assoc{id = Assoc_name, labels = Assoc_labels}, Labels, Assocs) ->
    Assocs2 = Assocs#{Assoc_name => sets:from_list(Assoc_labels)},
    Labels2 = lists:foldl(fun(Label, Acc) ->
        add_to_labels(Assoc_name, Label, Acc)
    end, Labels, Assoc_labels),
    {Labels2, Assocs2}.

-spec add_assoc(Assoc :: #assoc{}, Assocs :: map(), Labels :: map()) ->
    {Labels2 :: map(), Assocs2 :: map()} | {error, Reason :: string()}.
add_assoc(#assoc{id = Assoc_name} = Assoc, Labels, Assocs) ->
    case maps:find(Assoc_name, Assocs) of
        % New association
        error ->
            add_to_assocs(Assoc, Labels, Assocs);
        % Association already exists
        {ok, Assoc_name} ->
            {error, already_exists}
    end.

-spec add_assocs(Add_assocs :: list(#assoc{}), Labels :: map(), Assocs :: map()) ->
    {Labels2 :: #{atom() => [atom()]}, Assocs2 :: #{atom() => #assoc{}}, Failed :: [string()]}.
add_assocs(Add_assocs, Labels, Assocs) ->
    lists:foldl(fun(Assoc, {L, A, F}) ->
        case add_assoc(Assoc, L, A) of
            {error, Reason} ->
                Msg = io:format("add association '~p' '~p'", [Assoc, Reason]),
                {L, A, [Msg|F]};
            {L2, A2} ->
                {L2, A2, F}
        end
    end, {Labels, Assocs, []}, Add_assocs).

% LOOKUP
-spec lookup(Type :: assocs | labels, Lookup_entries :: [#assoc{}], Entries :: map()) -> map().
lookup(assocs, Lookup_assocs, Assocs) when is_list(Lookup_assocs), is_map(Assocs) ->
    lists:foldl(fun(Assoc_name, Acc) ->
        case maps:find(Assoc_name, Assocs) of
            {ok, Assoc_labels} -> Acc#{Assoc_name => Assoc_labels};
            error -> Acc
        end
    end, #{}, Lookup_assocs);
lookup(labels, Lookup_labels, Labels) when is_list(Lookup_labels), is_map(Labels) ->
    lists:foldl(fun(Label, Acc) ->
        case maps:find(Label, Labels) of
            {ok, Label_assocs} -> Acc#{Label => Label_assocs};
            error -> Acc
        end
    end, #{}, Lookup_labels).


% REMOVE
-spec cleanup_if_empty(Assocs :: reference(), Label :: atom(), Labels :: map()) -> map().
cleanup_if_empty(Assocs, Label, Labels) ->
    case sets:size(Assocs) of
        0 -> maps:remove(Label, Labels);
        _Size -> Labels#{Label => Assocs}
    end.

-spec remove_from_labels(Assoc_name :: atom(), Assoc_labels :: list(), Labels :: map()) -> map().
remove_from_labels(Assoc_name, Assoc_labels, Labels) ->
    lists:foldl(fun(L, Acc) ->
        case maps:find(L, Labels) of
            {ok, Assocs} ->
                Assocs2 = sets:del_element(Assoc_name, Assocs),
                cleanup_if_empty(Assocs2, L, Acc);
            error ->
                Acc
        end
    end, Labels, Assoc_labels).

-spec remove_assoc(Assoc_name :: atom(), Labels :: map(), Assocs :: map()) ->
    {map(), map()} | error.
remove_assoc(Assoc_name, Labels, Assocs) ->
    case maps:find(Assoc_name, Assocs) of
        {ok, Assoc_labels} ->
            Labels2 = remove_from_labels(Assoc_name, sets:to_list(Assoc_labels), Labels),
            Assocs2 = maps:remove(Assoc_name, Assocs),
            {Labels2, Assocs2};
        error -> error
    end.

-spec remove_assocs(Assoc_names :: list(atom()), Labels :: map(), Assocs :: map()) ->
    {Labels2 :: #{atom() => [atom()]}, Assocs2 :: #{atom() => #assoc{}}, Failed :: [string()]}.
remove_assocs(Remove_assocs, Labels, Assocs) ->
    lists:foldl(fun(Assoc_name, {L, A, F}) ->
        case remove_assoc(Assoc_name, L, A) of
            {Labels2, Assocs2} ->
                {Labels2, Assocs2, F};
            error ->
                Msg = io:format("remove association '~p' undefined", [Assoc_name]),
                {L, A, [Msg| F]}
        end
    end, {Labels, Assocs, []}, Remove_assocs).

% UPDATE
-spec update_assoc(Assoc :: #assoc{}, Labels :: map(), Assoc :: map()) ->
    {map(), map()} | {error, {undefined, string()}}.
update_assoc(#assoc{id = Assoc_name} = Assoc, Labels, Assocs) ->
    case remove_assoc(Assoc_name, Labels, Assocs) of
        {Labels2, Assocs2} ->
            add_assoc(Assoc, Labels2, Assocs2);
        error ->
            {error, {undefined, Assoc_name}}
    end.

-spec update_assocs(Update_assocs :: [#assoc{}], Labels :: map(), Assocs :: map()) ->
    {Labels2 :: #{atom() => [atom()]}, Assocs2 :: #{atom() => #assoc{}}, Failed :: [string()]}.
update_assocs(Update_assocs, Labels, Assocs) ->
    lists:foldl(fun(Assoc, {L, A, F}) ->
        case update_assoc(Assoc, L, A) of
            {error, Reason} ->
                Msg = io:format("update association '~p' '~p'", [Assoc, Reason]),
                {L, A, [Msg| F]};
            {Labels2, Assocs2} ->
                {Labels2, Assocs2, F}
        end
    end, {Labels, Assocs, []}, Update_assocs).
