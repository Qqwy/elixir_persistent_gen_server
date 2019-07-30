# PersistentGenServer

PersistentGenServer makes your Generic Elixir (or Erlang) Servers Persistent!

It is currently in a relatively early development stage. As such, it is not yet available on Hex.

## Example

At its essence, the only thing you need to do to make your GenServer persistent, is to replace the line

```elixir
GenServer.start(YourModule, list_of_arguments, maybe_some_options)
```

with

```elixir
PersistentGenServer.start(YourModule, list_of_arguments, maybe_some_options)
```


## What does it do?

A Persistent GenServer will behave just like a normal GenServer, with the following differences:

- It will keep track of the state changes that are the result of every call/cast made to it, and store these in the configured persistence layer (Which implements the `PersistentGenServer.Storage` behaviour)
- It will automatically stop after a specified timeout.
- If it is called after it has been stopped, a new server with the last persisted state will be started.
- Only in the case of a normal shutdown, will the process' state be removed from the persistent storage. As such, many kinds of crashes will result in the process being restarted with the last state before the crash.

## Why is this useful?

Many systems are more easily modeled as a bunch of (potentially communicating) state machines, rather than modelling them around a relational database. 

As a simple example, consider a (long-running) game: Handling player inputs is something that is much more natural in a state machine than it is with wrapping a database. However, we do (1) want to persist what the player does, but (2) not keep a GenServer running for all players, since only a fraction of those is playing at any single given time.

After having encountered about three situations in which I was re-implementing a very similar 'persistence' layer for my GenServers, I decided to extract this logic, and make it more general by hiding the persistency logic as much as possible from the user of the system.

## How does it work?

Internally, a special process registry (which wraps the Elixir.Registry by default, but can use any other registry you desire as well) keeps track of which processes are currently started, and which only exist as data persisted to disk.

You do not receive a PID as response from `PersistentGenServer.start`. Rather, you receive a _symbolic PID_ that includes all information to start the server again at a later time. (Essentially the arguments to PersistentGenServer.start).

## Configuration

PersistentGenServer is configured through [Specify](https://github.com/Qqwy/elixir_specify) and as such can be configured in many different ways, and multiple differently-configured variants of the PersistentGenServer can run alongside each-other.


## To Dos before stable release

  - [ ] Stateful property tests to make sure there are no race conditions.
  - [x] Swap out process registries that PersistentGenServer.Registry wraps.
  - [ ] Let users choose between: (temporary/transient/permanent)
    - Wipe persistency for GenServer when it stops normally or crashes.
    - Wipe persistency for GenServer only when it stops normally.
    - Even restart GenServer from persistency when it crashed before.
  - [x] Other storage adapters.
  - [x] Timeout length before a process petrifies itself.
  - [ ]  Configurably, only write to cache on `terminate` vs during each handle_* for efficienty vs fault-tolerancy?
  - [ ] A mapping function between the actual state and the state-to-be-persisted/reloaded, to hide ephemeral parts.
  - [ ] Improving the documentation and examples.

## Installation

PersistentGenServer is not yet available in Hex, and as such can be added to your deps as follows:

```elixir
{:persistent_gen_server, git: "https://github.com/Qqwy/elixir_persistent_gen_server"}
```

