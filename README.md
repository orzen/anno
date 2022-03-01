Anno(tate)
=====

**This project is currently in early development**

A process registry which lets you decorate Erlang Nodes with tags to control
spawn patterns.

Can be added during startup from config or during runtime via API.

Features (planned)
------------------

- Groups messages (label groups)
- Spawn patterns
- Resource balancing
- Support regex to apply lables to matching node names from the config.


Build
-----

    $ rebar3 compile


Debug
-----

### Eunit
```
rebar3 as test shell
debugger:start().
eunit:test(test_mod, [verbose]).
```

TODO
----

### Node related

- [ ] Apply predefined labels from config for static nodes

### Process related

