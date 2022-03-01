-module(anno_nodes).
-behavior(gen_server).

-ifdef(TEST).
-compile(export_all).
-endif.

-export([
    associate_sync/1,
    disassociate_sync/1,
    connect_sync/1,
    disconnect_sync/1,
    list_all/0,
    start_link/1,
    stop/0
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
%
% Anno will try to reconnect nodes that disconnect without an explicit action
% from an admin if the node has been connected to the cluster.

% TODO
% - Fix spec return values

-include_lib("kernel/include/logger.hrl").
-include("gen_server_log.hrl").
-include("types.hrl").
-include("modules.hrl").

-define(SERVER, {global, ?MODULE}).

% nodes {Expected_state, Current_state}
-record(state, {auto_connect :: boolean(),
                nodes :: #{atom() => {atom(), atom()}}}).

%% API %%

% connect will connect the node to the cluster
-spec connect_sync(Nodes :: [#node{}]) -> ok.
connect_sync(Nodes) when is_list(Nodes) ->
    gen_server:call(?SERVER, {connect, Nodes}).

-spec disconnect_sync(Nodes :: [atom()]) -> ok.
disconnect_sync(Nodes) when is_list(Nodes) ->
    gen_server:call(?SERVER, {disconnect, Nodes}).

% associate will add associations about future nodes and will not try to
% connect them to the cluster.
-spec associate_sync(Nodes :: [#node{}]) -> ok.
associate_sync(Nodes) when is_list(Nodes) ->
    gen_server:call(?SERVER, {associate, Nodes}).

disassociate_sync(Nodes) when is_list(Nodes) ->
    gen_server:call(?SERVER, {disassociate, Nodes}).

list_all() ->
    gen_server:call(?SERVER, {list, all}).

start_link(Nodes) ->
    gen_server:start_link(?SERVER, ?MODULE, Nodes, []).

stop() ->
    gen_server:stop(?SERVER).

% FEATURES
% - Calls world which will read and connect hosts from .hosts.erlang
% - Tracks nodes that connect to the cluster
% - Reconnect to nodes that once had a connection

%% CALLBACK %%

% Nodes = [{some_nodename, [
%     {matching, [{ipv4,"192.168.0.2"}]},
%     {labels, [labelx, labely]}
% ]}]
init(Cfg) ->
    ?GS_INIT(Cfg),

    Auto_connect = case proplists:lookup(auto_connect, Cfg) of
        {auto_connect, true} -> true;
        _Other -> false
    end,

    Cfg_nodes = case proplists:lookup(nodes, Cfg) of
        none -> [];
        N -> N
    end,

    % Shape config data
    Cfg_nodes2 = lists:foldl(fun({Node_name, Node_cfg}, Acc) ->
        Labels = case propslists:lookup(label, Node_cfg) of
            none -> [];
            L -> L
        end,

        % TODO add match

        [#node{name = Node_name, labels = Labels, connect = Auto_connect}| Acc]
    end, [], Cfg_nodes),

    Nodes = sets:from_list(Cfg_nodes2),

    World_nodes = case net_adm:world() of
        {error, _Reason} -> [];
        N2 -> N2
    end,

    % Merge nodes from configs and world
    Nodes2 = lists:foldl(fun(Node_name, Acc) ->
        sets:add_element(#node{name = Node_name, labels = [], connect = Auto_connect}, Acc)
    end, Nodes, World_nodes),

    Nodes3 = sets:to_list(Nodes2),

    Assocs = nodes_to_assocs(Nodes3),
    associate(Assocs),

    % TODO connect nodes if auto_connect is set and add retry
    {Connected, Failed} = log_connect_nodes(Nodes3),

    case net_kernel:monitor_nodes(true, [{node_type, all}]) of
        ok ->
            {ok, #state{auto_connect = Auto_connect, nodes = Nodes}, hibernate};
        Reason2 ->
            ?LOG_ERROR("failed to monitor nodes '~p'", [Reason2]),
            {stop, "failed to monitor nodes"}
    end.

terminate(Reason, _State) ->
    ?GS_TERM(Reason),
    ok.

% Add cluster will invite nodes. Nodes that join the cluster will be detected
% by nodeup events.
% Nodes :: [#node{}]

% CALL
handle_call({connect, Connect_nodes}, _From,  #state{nodes = Nodes} = State) ->
    {Connected_nodes, Failed_nodes} = connect_nodes(Connect_nodes),
    log_failed(connect_nodes, Failed_nodes),

    associate(Connected_nodes),

    New_nodes = to_map(Connected_nodes),
    Nodes2 = maps:merge(Nodes, New_nodes),

    {noreply, State#state{nodes = Nodes2}, hibernate};
handle_call({disconnect, Disconnect_names}, _From, #state{nodes = Nodes} = State) ->
    {Disconnected_names, Failed_names} = disconnect_nodes(Disconnect_names),
    log_failed(disconnect_nodes, Failed_names),

    Nodes2 = lists:foldl(fun(X, Acc) ->
        maps:remove(X, Acc)
    end, Nodes, Disconnected_names),

    disassociate(Disconnected_names),

    {noreply, State#state{nodes = Nodes2}, hibernate};
handle_call({associate, Associate_nodes}, _From, State) ->
    associate(Associate_nodes),
    {noreply, State, hibernate};
handle_call({disassociate, Disassociate_names}, _From, State) ->
    disassociate(Disassociate_names),
    {noreply, State, hibernate};
handle_call({update, Update_nodes}, _From,  State) ->
    update(Update_nodes),
    {noreply, State, hibernate};
handle_call({list, all}, _From, #state{nodes = Nodes} = State) ->
    {_, Assocs} = ?ASSOC:list_all(?NODE_ASSOC),
    {reply, {ok, [{nodes, Nodes}, {assocs, Assocs}]}, State, hibernate};
handle_call(Req, From, State) ->
    ?GS_UNHANDLED_CALL(Req, From),
    {noreply, State, hibernate}.

% CAST
handle_cast(Req, State) ->
    ?GS_UNHANDLED_CAST(Req),
    {noreply, State, hibernate}.

% INFO
handle_info({nodeup, Node_name, _Info}, #state{auto_connect = Auto_connect, nodes = M_nodes} = State) ->
    ?LOG_NOTICE("nodeup '~p'", [Node_name]),

    Nodes2 = case Auto_connect of
        true ->
            {Connected_nodes, Failed_nodes} = connect_nodes(Connect_nodes) ;
        false -> ok
    end,
    % TODO check if node associations already exists, otherwise this will override existing associations.
    {Labels2, Nodes2} = add_nodes([#node{name = Node_name, labels=[]}], M_labels, M_nodes, false),
    {noreply, State#state{labels = Labels2, nodes = Nodes2}, hibernate};
handle_info({nodedown, Node_name, _Info}, #state{labels = Labels, nodes = Nodes} = State) ->
    ?LOG_NOTICE("nodedown '~p'", [Node_name]),
    % TODO only remove associations if there is none otherwise of the node is shaky this will clear associations unintensionally.
    {Labels2, Nodes2} = remove_nodes([Node_name], Labels, Nodes, false),
    {noreply, State#state{labels = Labels2, nodes = Nodes2}, hibernate};
handle_info(Msg, State) ->
    ?GS_UNHANDLED_INFO(Msg),
    {noreply, State, hibernate}.

%% INTERNAL %%

associate(Nodes) ->
    Node_assocs = nodes_to_assocs(Nodes),
    case ?ASSOC:associate_sync(?NODE_ASSOC, Node_assocs) of
        ok -> [];
        {error, Failed} ->
            log_failed(associate_sync, Failed),
            []
    end.

disassociate(Names) ->
    case ?ASSOC:disassociate_sync(?NODE_ASSOC, Names) of
        ok -> [];
        {error, Failed} ->
            log_failed(disassociate_sync, Failed),
            []
    end.

update(Nodes) ->
    Node_assocs = nodes_to_assocs(Nodes),
    case ?ASSOC:update_sync(?NODE_ASSOC, Node_assocs) of
        ok -> [];
        {error, Failed} ->
            log_failed(update_sync, Failed),
            []
    end.

-spec to_map(Nodes :: [#node{}]) -> map().
to_map(Nodes) ->
    lists:foldl(fun(#node{name = Node_name} = X, Acc) ->
        Acc#{Node_name => ok}
    end, #{}, Nodes).

-spec log_failed(Func :: atom(), Entries :: string()) -> ok.
log_failed(Func, Entries) ->
    lists:each(fun(X) ->
        ?LOG_WARNING("~p: failed ~p", [Func, X])
    end, Entries).

-spec nodes_to_assocs(Nodes :: [#node{}]) -> [#assoc{}].
nodes_to_assocs(Nodes) ->
    [#assoc{id = Node_name, labels = Node_labels} ||
     #node{name = Node_name, labels = Node_labels} <- Nodes].

-spec do_disconnect(Node_name :: node()) -> true | false | ignored.
do_disconnect(Node_name) -> erlang:disconnect(Node_name).

% Named disc_node instead of disconnect_node to not conflict with the stdlib
-spec disc_node(Node_name :: node()) -> boolean() | string().
disc_node(Node_name) when is_atom(Node_name) ->
    case do_disconnect(Node_name) of
        true -> true;
        false -> io:format("disconnect node '~p'", [Node_name]);
        ignored -> io:format("disconnect node (unreachable) '~p'", [Node_name])
    end.

-spec disconnect_nodes(Node_names :: [node()]) ->
    {Disconnected_nodes :: [node()], Failed_nodes :: [node()]}.
disconnect_nodes(Node_names) ->
    lists:foldl(fun(Node_name, {Disconnected, Failed}) ->
        case disc_node(Node_name) of
            true -> {[Node_name|Disconnected], Failed};
            Reason -> {Disconnected, [Reason|Failed]}
        end
    end, {[], []}, Node_names).

-spec do_connect(Node_name :: node()) -> pong | pang.
% TODO net_kernel:connect_node(Node_name).
% Node_name example 'foo@1.2.3.4'
do_connect(Node_name) -> net_adm:ping(Node_name).

-spec connect_nodes(Nodes :: [#node{}]) -> {Connected_nodes :: list(), Failed_nodes :: list()}.
connect_nodes(Nodes) ->
    lists:foldl(fun(#node{name = Node_name}, {Connected, Failed}) ->
        case do_connect(Node_name) of
            pong -> {[Node_name|Connected], Failed};
            pang ->
                Log = io:format("connect node '~p'", [Node_name]),
                {Connected, [Log|Failed]}
        end
    end, {[],[]}, Nodes).

log_connect_nodes(Nodes) ->
    {Connected, Failed} = connect_nodes(Nodes),
    log_failed(log_connect_nodes, Failed),
    {Connected, Failed}.
