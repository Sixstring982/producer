# Producer

_Producer_ is an OCaml library for building values using a monadic dependency
graph.

Here's an example:

```ocaml
(** [int_node] provides an integer value to the rest of the graph.

    The entire graph takes a "context", which is an input given to each
    node when the graph executes. Each graph must use the same context
    type for all nodes.

    The context type of this graph is [unit].
  *)
let int_node: (unit, int) Producer.Node.t =
  Producer.Node.make
    []              (* This node takes no dependencies *)
    (fun () -> 123) (* This node produces the value [123]. *)

(** [string_node] provides the stringified output of [int_node] to the
    rest of the graph. *)
let string_node: (unit, int) Producer.Node.t =
  Producer.Node.make
    [int_node]                    (* This node depends on [int_node] *)
    (fun () n -> string_of_int n) (* [n] is the output of [int_node]. *)

(** [graph] wraps [string_node] and makes it executable. *)
let graph = Producer.Graph.make string_node

(** [graph] can be [execute]d to produce an output. *)
let () = assert (graph.execute () = "123")
```

This is a flexible way to re-use code -- consider the following example which
authenticates a request and provides a "user" type, which can be used by other
nodes in the graph directly:

```ocaml
module Producer = Producer.Make(Lwt_result)

let user_producer: (< request: Cohttp.Request.t; .. >, User.t) Producer.Node.t =
  Producer.Node.make [] @@ fun context -> 
    match Authentication.authenticate_request context#request with
      | Error _ -> Lwt_result.fail `Not_authenticated
      | Ok user -> Lwt_result.return user

let user_profile_producer: (< .. >, Profile.t) Producer.Node.t =
  Producer.Node.make [user_producer] @@ fun _context user ->
    let (let+) = Lwt.bind in
    let+ profile = Database.profile_for_user user in
    match profile with
      | Error _ -> Lwt_result.fail `Profile_not_found
      | Ok profile -> Lwt_result.return user

(* Results from the above nodes are cached -- feel free to re-use them 
   in other nodes! *)
let produce_response: (< .. >, User_profile_response.t) Producer.Node.t =
  Producer.Node.make [user_producer; user_profile_producer] @@
    fun _context user profile -> { user; profile }
```

# Features

## Caching

Each node in the graph is run only once per call to `execute`. If multiple
nodes depend on a node `n`, `n` is only called once -- its result is cached and
re-used.

## Bring your own monad

The `Producer.Make` functor accepts any monad for your effects, which are
automatically handled when a graph node executes.

Asynchronous monads like `Lwt`, `Lwt_result`, or `Eio.Promise` are a
popular choice.

## Compile-time satisfiability

Unlike some other graph libraries which depend on reflection to link
dependencies at runtime, A producer graph is guaranteed to be satisfied at
compile time.

## Structurally-typed context

A graph can use an OCaml object as its context, which allows each node to take
different context types. OCaml will properly infer the context when executing
the graph:

```ocaml
(** [int_node] takes no context (but must be typed as an open object). *)
let int_node : (< .. >, int) Producer.Node.t =
  Producer.Node.make [] (fun _ -> 123)

(** [mul_node] needs some multipier value from the context. *)
let mul_node : (< multiplier : int ; .. >, int) Producer.Node.t =
  Producer.Node.make [ int_node ] (fun c n -> c#multiplier * n)

(** [string_node] needs some tag value from the context. *)
let string_node : (< tag : string ; .. >, string) Producer.Node.t =
  Producer.Node.make [ int_node; mul_node ] (fun c n m ->
      Format.sprintf "[%s]: int_node = %d, mul_node = %d" c#tag n m)

(** [execute] is inferred to require all of the structurally typed context
    dependencies above -- it's a compilation error to provide an object which
    does not have all of the below methods! *)
let actual = (Producer.Graph.make string_node).execute (object
   method multiplier = 10
   method tag = "TAG"
end)

let () = assert (actual = "[TAG]: int_node = 123, mul_node = 1230")
```

