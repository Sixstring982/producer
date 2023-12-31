(** Monadic type, used to control effects (usually asynchronous execution) in a producer graph. *)
module type MONAD = sig
  type 'a t

  val return : 'a -> 'a t
  (** [return a] lifts the value [a] into the monadic context. *)

  val bind : 'a t -> ('a -> 'b t) -> 'b t
  (** [bind ma fn] applies the monadic function [fn] to the monadic value [ma]. *)
end

(** Monadic type, with two type parameters. *)
module type MONAD2 = sig
  type ('a, 'b) t

  val return : 'a -> ('a, 'b) t
  (** [return a] lifts the value [a] into the monadic context. *)

  val bind : ('a, 'b) t -> ('a -> ('c, 'b) t) -> ('c, 'b) t
  (** [bind ma fn] applies the monadic function [fn] to the monadic value [ma]. *)
end

exception Cyclic
(** Raised when constructing a graph if it contains a cycle. *)

(** Creates a new Producer library targeting the given monadic type. *)
module Make (M : MONAD) : sig
  (** Dependencies of a producer node. *)
  module rec Dependencies : sig
    (** GADT representing a list of producer node dependencies. *)
    type (_, _, _) t =
      | [] : ('context, 'output, 'output) t
      | ( :: ) :
          ('context, 'a) Node.t * ('context, 'b, 'output) t
          -> ('context, 'a -> 'b, 'output) t
  end

  (** Nodes in a producer graph. *)
  and Node : sig
    type ('context, 'output) t

    val make :
      ('context, 'deps, 'output M.t) Dependencies.t ->
      ('context -> 'deps) ->
      ('context, 'output) t
  end

  (** Encapsulated graph of producer nodes. *)
  module Graph : sig
    type ('context, 'output) t = { execute : 'context -> 'output M.t }

    val make : ('context, 'output) Node.t -> ('context, 'output) t
    (** [make nodes output_node] creates a [Graph.t] out of the given nodes, marking [output_node] 
        as the Node which produces the graph's output.

        @raise [Cyclic] if a dependency cycle is found somewhere within the graph. *)
  end
end

(** Creates a new Producer library with the given monadic type (which has two type parameters). *)
module Make2 (M : MONAD2) : sig
  (** Dependencies of a producer node. *)
  module rec Dependencies : sig
    (** GADT representing a list of producer node dependencies. *)
    type (_, _, _, _) t =
      | [] : ('context, 'output, 'output, 'e) t
      | ( :: ) :
          ('context, 'a, 'e) Node.t * ('context, 'b, 'output, 'e) t
          -> ('context, 'a -> 'b, 'output, 'e) t
  end

  (** Nodes in a producer graph. *)
  and Node : sig
    type ('context, 'output1, 'output2) t

    val make :
      ('context, 'deps, ('output1, 'output2) M.t, 'output2) Dependencies.t ->
      ('context -> 'deps) ->
      ('context, 'output1, 'output2) t
  end

  (** Encapsulated graph of producer nodes. *)
  module Graph : sig
    type ('context, 'output1, 'output2) t = { execute : 'context -> ('output1, 'output2) M.t }

    val make : ('context, 'output1, 'output2) Node.t -> ('context, 'output1, 'output2) t
    (** [make nodes output_node] creates a [Graph.t] out of the given nodes, marking [output_node] 
        as the Node which produces the graph's output.

        @raise [Cyclic] if a dependency cycle is found somewhere within the graph. *)
  end
end

module Sync : MONAD with type 'a t = 'a
include module type of Make (Sync)
