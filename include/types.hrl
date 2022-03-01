-record(assoc, {id :: atom(),
                labels :: list()}).

-record(node, {name = undefined :: atom(),
               labels = [] :: list(),
               connect = false :: boolean(),
               connection = 0 :: ok | integer()}). % integer is a retry timer
