-module(anno_nodes).
-behavior(gen_server).

-export([add_nodes/1,
         list_labels/0,
         list_nodes/0,
         remove_nodes/1,
         update_nodes/1]).

-export([
         start_link/0,
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
-define(SERVER, {global, ?MODULE}).

-record(state, {t_nodes :: atom(),
                t_labels :: atom()}).

-record(node, {name :: node(),
               labels :: list()}).

%% API %%
add_nodes(Nodes) when is_list(Nodes) ->
    gen_server:cast(?SERVER, {add_nodes, Nodes}).

list_labels() ->
    gen_server:call(?SERVER, {list_nodes}).

list_nodes() ->
    gen_server:call(?SERVER, {list_nodes}).

remove_nodes(Nodes) when is_list(Nodes) ->
    gen_server:call(?SERVER, {remove_nodes, Nodes}).

% List with #node{} e.g. [{node, [labelX, labelY]}]
update_nodes(Nodes) when is_list(Nodes) ->
    gen_server:call(?SERVER, {update_labels, Nodes}).

%% CALLBACK %%

start_link() ->
    gen_server:start_link(?SERVER, ?MODULE, [], []).

init([]) ->
    % List of all labels key == label name, value == label table
    T_labels = ets:new(anno_labels, [named_table, bag]),

    % Table with all nodes, to display a nodes labels
    T_nodes = ets:new(anno_nodes, [named_table, bag]),

    case net_kernel:monitor_nodes(true, [{node_type, all}]) of
        ok -> {ok, #state{t_nodes = T_nodes, t_labels = T_labels}, hibernate};
        Reason ->
            ?LOG_ERROR("failed to monitor nodes '~p'", [Reason]),
            {stop, "failed to monitor nodes"}
    end.

terminate(Reason, _State) ->
    ?LOG_INFO("terminate '~p'", [Reason]),
    ok.

handle_call(Req, From, State) ->
    ?LOG_ERROR("unhandled call from '~p', '~p'", [From, Req]),
    {noreply, State, hibernate}.

% Add cluster will invite nodes. Nodes that join the cluster will be detected
% by nodeup events.
handle_cast({add_nodes, Nodes}, State) ->
    F = fun(#node{name = Node_name} = Node) ->
        add_node(Node),
        connect_node(Node_name)
    end,
    lists:foreach(F, Nodes),

    {noreply, State, hibernate};
% Nodes::[atom()]
handle_cast({remove_nodes, Nodes}, State) ->
    Each = fun(Node_name) ->
        % First remove the node so that it's not used while other work is done.
        remove_node(Node_name),
        % Disconnect the nodes
        disc_node(Node_name)
    end,
    lists:foreach(Each, Nodes),

    {noreply, State, hibernate};
% Nodes :: [#node{}]
handle_cast({update_nodes, Nodes}, State) when is_list(Nodes) ->
    Each = fun(#node{name = Node_name} = Node) ->
        % First remove the node so that it's not used while other work is done.
        remove_node(Node_name),
        add_node(Node)
    end,
    lists:foreach(Each, Nodes),
    {noreply, State, hibernate};
handle_cast(list_labels, State) ->
    F = fun({Label_id, Nodes}, Acc) ->
            ?LOG_NOTICE("Label '~s' LABELS '~p'", [Label_id, Nodes]),
            Acc
        end,
    ets:foldl(F, [], anno_labels),
    {noreply, State};
handle_cast(list_nodes, State) ->
    F = fun({Node_id, Node}, Acc) ->
            ?LOG_NOTICE("NODE '~s' LABELS '~p'", [Node_id, Node]),
            Acc
        end,
    ets:foldl(F, [], anno_nodes),
    {noreply, State};
handle_cast(Req, State) ->
    ?LOG_ERROR("unhandled cast '~p'", [Req]),
    {noreply, State, hibernate}.

handle_info({nodeup, Node_name, _Info}, State) ->
    ?LOG_NOTICE("nodeup '~p'", [Node_name]),
    add_node(#node{name = Node_name, labels=[]}),
    {noreply, State, hibernate};
handle_info({nodedown, Node_name, _Info}, State) ->
    ?LOG_NOTICE("nodedown '~p'", [Node_name]),
    remove_node(Node_name),
    {noreply, State, hibernate};
handle_info(Msg, State) ->
    ?LOG_ERROR("unhandled info '~p'", [Msg]),
    {noreply, State, hibernate}.

%% INTERNAL %%

label_tid(Label) ->
    erlang:list_to_atom(io_lib:format("anno_~s", [Label])).

ensure_label_table(Label) ->
    Tid = label_tid(Label),
    case ets:member(anno_labels, Label) of
        false ->
            Tab = ets:new(Tid, [bag]),
            true = ets:insert(anno_labels, {Tid, Tab}),
            {ok, Tab};
        true ->
            [{Tid, Tab}] = ets:lookup(anno_labels, Tid),
            {ok, Tab}
    end.

get_label_tables(Labels) ->
    Fold = fun(Label) ->
       {ok, Tab} = ensure_label_table(Label),
       {Label, Tab}
    end,
    lists:foldl(Fold, [], Labels).

add_node_to_labels(Node, Labels) ->
    Each = fun(Tab) ->
        true = ets:insert(Tab, Node)
    end,
    lists:foreach(Each, Labels).

add_node(#node{name = Name, labels = Labels} = Node) ->
    Labels = get_label_tables(Labels),
    add_node_to_labels(Node, Labels),
    true = ets:insert(anno_nodes, {Name, Labels}).

remove_node_from_label(Tid, Node_name) ->
    Tab = ets:whereis(Tid),
    ets:delete(Tab, Node_name).

remove_empty_table(Tid) ->
    case ets:info(Tid, size) of
        0 -> ets:delete(Tid)
    end.

remove_node_from_labels(#node{name = Node_name, labels = Labels}) ->
    lists:foreach(fun(Label) ->
                    Tid = label_tid(Label),
                    remove_node_from_label(Tid, Node_name),
                    remove_empty_table(Tid)
                  end, Labels).

remove_node(Node_id) when is_atom(Node_id) ->
    % Lookup node to get the labels
    Node = ets:lookup(anno_nodes, Node_id),
    ets:delete(anno_nodes, Node_id),
    remove_node_from_labels(Node).

% Named disc_node instead of disconnect_node to not conflict with the stdlib
disc_node(Node) when is_atom(Node) ->
    case erlang:disconnect_node(Node) of
        true -> ok;
        false -> ?LOG_WARNING("failed to disconnect node '~p'", [Node]);
        ignored -> ?LOG_WARNING("failed to disconnect, node '~p' is unreachable", [Node])
    end.

% Node example 'foo@1.2.3.4'
connect_node(Node) when is_atom(Node) ->
    %net_kernel:connect_node(Node).
    case net_adm:ping(Node) of
        pang -> ?LOG_WARNING("failed to connect node '~p'", [Node]);
        pong -> ok
    end.
