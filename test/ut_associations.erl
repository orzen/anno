-module(ut_associations).

-include_lib("eunit/include/eunit.hrl").
-include("types.hrl").

-define(ASSOC, anno_associations).


add_assoc() ->
    Add_assoc = #assoc{id = n1, labels = [l1, l2]},

    In_labels = #{l4 => sets:from_list([n9])},
    In_assocs = #{n9 => sets:from_list([l4])},

    % Expected result
    Res_labels = #{l1 => sets:from_list([n1]),
                   l2 => sets:from_list([n1]),
                   l4 => sets:from_list([n9])},
    Res_assocs = #{n1 => sets:from_list([l1,l2]),
                  n9 => sets:from_list([l4])},

    {Out_labels, Out_assocs} = ?ASSOC:add_assoc(Add_assoc, In_labels, In_assocs),

    ?_assert(Res_labels == Out_labels),
    ?_assert(Res_assocs == Out_assocs).

add_assocs() ->
    Add_assocs = [#assoc{id = n1, labels = [l1, l2]},
                  #assoc{id = n2, labels = [l1, l3]}],
    In_labels = #{l4 => sets:from_list([n9])},
    In_assocs = #{n9 => sets:from_list([l4])},

    % Expected result
    Res_labels = #{l1 => sets:from_list([n2,n1]),
                   l2 => sets:from_list([n1]),
                   l3 => sets:from_list([n2]),
                   l4 => sets:from_list([n9])},
    Res_assocs = #{n1 => sets:from_list([l1,l2]),
                   n2 => sets:from_list([l1,l3]),
                   n9 => sets:from_list([l4])},

    {Out_labels, Out_assocs} = ?ASSOC:add_assocs(Add_assocs, In_labels, In_assocs),

    ?_assert(Res_labels == Out_labels andalso Res_assocs == Out_assocs).

add_test_() ->
    {timeout, 120, [add_assoc(),
                    add_assocs()]}.

remove_from_labels() ->
    % Input
    Assoc_name = n1,
    Assoc_labels = [l1, l2],
    Labels = #{l1 => sets:from_list([n2, n1]),
               l2 => sets:from_list([n1])},

    % Expected output
    E_labels = #{l1 => sets:from_list([n2])},

    O_labels = ?ASSOC:remove_from_labels(Assoc_name, Assoc_labels, Labels),

    ?_assert(E_labels == O_labels).

remove_assocs() ->
    Remove_assocs = [n2, n3],
    In_labels = #{l1 => sets:from_list([n2]),
                  l2 => sets:from_list([n2,n3]),
                  l4 => sets:from_list([n1])},
    In_assocs = #{n1 => sets:from_list([l4]),
                  n2 => sets:from_list([l1,l2]),
                  n3 => sets:from_list([l2])},

    % Expected result
    Res_labels = #{l4 => sets:from_list([n1])},
    Res_assocs = #{n1 => sets:from_list([l4])},

    {Out_labels, Out_assocs} = ?ASSOC:remove_assocs(Remove_assocs, In_labels, In_assocs),

    ?_assert(Res_labels == Out_labels andalso Res_assocs == Out_assocs).

remove_test_() ->
    {timeout, 120, [remove_from_labels(),
                    remove_assocs()]}.

lookup_labels() ->
    Labels = #{l1 => [n1, n2], l2 => [a, b], l3 => [n3, n4]},
    Lookup_labels = [l1, l3],

    E_labels = #{l1 => [n1, n2], l3 => [n3, n4]},

    O_labels = ?ASSOC:lookup(labels, Lookup_labels, Labels),

    ?_assert(E_labels == O_labels).


lookup_assocs() ->
    Assocs = #{l1 => [n1, n2], l2 => [a, b], l3 => [n3, n4]},
    Lookup_assocs = [l1, l3],

    E_assocs = #{l1 => [n1, n2], l3 => [n3, n4]},

    O_assocs = ?ASSOC:lookup(assocs, Lookup_assocs, Assocs),

    ?_assert(E_assocs == O_assocs).

lookup_test_() ->
    {timeout, 120, [lookup_labels(), lookup_assocs()]}.
