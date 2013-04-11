# Maelstrom

Is a distributed, fault-tolerant, and soft real-time computation
platform. As opposed to Map/Reduce, Maelstrom handles its units of
computation in streams, in parallel across a cluster, and concurrently
on each node.

Maelstrom is sovereign (master-less) and therefore has not a single
point of failure.

The goal is to provide a generalized framework for parallel,
concurrent, and distributed computational work-loads that are
non-trivial and can be represented as a *stream of data*, the datum as
a tuple.

# Design Rationale

I chose to work on this using Haskell for a number of reasons; as I
began learning the language its design elegance weighed heavily on
what I had previously thought was *expressive*. The type system in
Haskell is unmatched and the kind of thought you can express with the
language as a result of this is deep and rich.

On a more practical note: I chose the language because the type system
provides remarkable safety (such a necessity in distributed systems!),
green threads (light weight like Erlang's processes), Software
Transactional Memory, Haskell is compiled, and a rich collection of
available libraries.

Opposed to Erlang, Haskell does fall short in that there is no OTP
equivalent, I/O is markedly slower in Haskell than in Erlang's BEAM
VM, and concurrency/distributed features are in a nascent stage (ala
**Cloud-Haskell**).

I do not wish to emulate Erlang's concurrency model and will therefore
not be using Cloud-Haskell.

# Architecture

Forthcoming...
