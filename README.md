Maelstrom
=========

A distributed, fault-tolerant, and soft real-time computation platform. Instead of parallel computing in batches (Map/Reduce) Maelstrom handles its units of computation in streams, in parallel, and distributed in a cluster.

Maelstrom is master-less and therefore has no single point of failure.

And yes, it is in development.

Design Rationale
================

[Erlang/OTP](http://www.erlang.org/) are well-suited for distributed, fault-tolerant, and highly-concurrent computing. Distributed workflows and primitives (built into the Erlang VM), hot-code loading, concurrency primitives, supervision trees, and many other features make Erlang/OTP a great fundamental building block because these features are part of the language and tool chain, don't need to be bolted on, and the stack has been around for over 20 years (__industry proven__).

The Maelstrom architecture, to achieve true high-availability and parallel _and concurrent_ computing, is borrowing from both the masterless model of Amazon's Dynamo (as seen in [Riak](http://wiki.basho.com/Riak.html) and [Cassandra](http://cassandra.apache.org/)) and the seed/peer model pioneered by the BitTorrent protocol.

Architecture
============

Each node is capable of receiving a _topo_, these describe a graph topology of input streams, computing Vertexes (which can output streams that other vertexes consume as well), and output streams. At the time of submission, in the topo spec, an _n_-value is specified to inform the receiving node of how many peer nodes in the cluster should "seed" the topo (this is how the cluster can recover a submitted topo of a failed node that had ownership). The node that owns the topo is responsible for unit acking (if they are specified in the topo spec). The topo state is always replicated between the seeds and the owner to maintain consistency in the event of ownership hand-off.

The cluster gossips about its state, every node informs the other nodes about its availability; if the cluster is setup as a load-balanced worker pool then each node will attempt to divide its work up with all of the other nodes. As a node's resources become available more units from the input stream from the seeds are requested &c... Nodes will also gossip about topo submissions and seed responsibilities. Each node can be configured with an _r_-value to specify how many topos it is allowed to be responsible for as a seed; if more topos are submitted or seeded to the node than it is configured to handle, it will hand-off its responsibility to the next available node. Every node is only allowed to have __ownership__ of one topo. If all nodes are maxed out on their topos and seeds then the cluster will enqueue the topos till more resources become available.

Clusters at the time of configuration can either be setup more like a priority queue or as a load-balanced worker pool.

Topologies (topos)
------------------

Topologies are configured using a similar approach to the OTP supervisor child specs. You, most importantly, specify the graph of input streams, Vertexes, and output streams; you can also configure the _n_-value for the topo, whether or not units of computation for the topo should be acked (disabling it decreases latency but you also lose guarantees). A unit of computation is broken down into a tuple pair.

When writing the topo spec you can either provide a ModFunArg for the Vertex (if the code has been deployed and compiled on all the nodes [less startup time]) or a FileMod | FunArg to be packaged and distributed across the cluster (slower startup time).


* Input streams can be anything that provides a source of data. It could be a push-notification system, the Twitter firehose, an RSS feed, a database, &c...
* A Vertex is a concrete unit of computation, it receives a tuple from an input stream (vertexes are configured a bit like "type classes", this matches vertexes with input streams) and outputs as a stream or to a database (or whatever the user chooses).
* An output stream is just a tuple pair output from a Vertex - any Vertex can be configured to receive the output stream of a Vertex as its input stream.
