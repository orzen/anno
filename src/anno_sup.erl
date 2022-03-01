%%%-------------------------------------------------------------------
%% @doc anno top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(anno_sup).

-behaviour(supervisor).

-export([start_child/1,
         start_link/2]).
-export([init/1]).

-include_lib("kernel/include/logger.hrl").
-include("modules.hrl").
-include("gen_server_log.hrl").

-define(SERVER, {global, ?MODULE}).
-define(CHILD_ID(I, M, F, A), #{id => I, start => {M, F, A}}).
-define(CHILD(M, F, A), ?CHILD_ID(M, M, F, A)).

% Embedd list in arguments for nodes and modules because the expect arguments.
% While registry does not take any arguments and will therefore need an empty
% list.
-define(CHILD_ASSOCIATIONS(A), ?CHILD_ID(A, anno_associations, start_link, [A])).
-define(CHILD_NODES(A), ?CHILD(anno_nodes, start_link, [A])).
-define(CHILD_MODULES(A), ?CHILD(anno_modules, start_link, [A])).
-define(CHILD_REGISTRY(), ?CHILD(anno_registry, start_link, [])).

-define(INIT_DATA(X, Y), [{nodes, X}, {modules, Y}]).

start_child(Child) ->
    supervisor:start_child(?SERVER, Child).

start_link(Nodes, Modules) ->
    supervisor:start_link(?SERVER, ?MODULE, ?INIT_DATA(Nodes, Modules)).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional

init(?INIT_DATA(Cfg_nodes, Cfg_modules)) when is_list(Cfg_nodes), is_list(Cfg_modules) ->
    ?GS_INIT(?INIT_DATA(Cfg_nodes, Cfg_modules)),

    SupFlags = #{strategy => one_for_all, intensity => 0, period => 1},

    % Auto start
    Node_assoc = ?CHILD_ASSOCIATIONS(?NODE_ASSOC),
    Mod_assoc = ?CHILD_ASSOCIATIONS(?MOD_ASSOC),
    Nodes = ?CHILD_NODES(Cfg_nodes),
    %Modules = ?CHILD_MODULES(Modules),
    Registry = ?CHILD_REGISTRY(),

    ChildSpecs = [Node_assoc,
                  Mod_assoc,
                  Nodes,
                  Registry],

    {ok, {SupFlags, ChildSpecs}}.
