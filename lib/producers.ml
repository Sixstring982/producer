module type MONAD = sig
  type 'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module Sync : MONAD = struct
  type 'a t = 'a

  let bind a fn = fn a
end

module Make (M : MONAD) = struct
  module Types = struct
    module rec Graph : sig
      type t = { nodes_by_key : Hmap.t }
    end =
      Graph

    and Execution_cache : sig
      type t = { outputs_by_key : Hmap.t }
    end =
      Execution_cache

    and Dependencies : sig
      type (_, _, _) t =
        | [] : ('context, 'output, 'output) t
        | ( :: ) :
            ('context, 'a) Node.t * ('context, 'b, 'output) t
            -> ('context, 'a -> 'b, 'output) t
    end =
      Dependencies

    and Node : sig
      type ('context, 'output) t =
        | Node : {
            dependencies : ('context, 'deps, 'output) Dependencies.t;
            produce : 'context -> 'deps;
            output_key : 'output Hmap.key;
            node_key : ('context, 'output) t Hmap.key;
          }
            -> ('context, 'output) t
    end =
      Node
  end

  module Dependencies = struct
    include Types.Dependencies
  end

  module Node = struct
    include Types.Node
    module Dependencies = Types.Dependencies

    let output_key : (_, 'output) t -> 'output Hmap.key = function
      | Node { output_key; _ } -> output_key

    let node_key : ('context, 'output) t -> ('context, 'output) t Hmap.key =
      function
      | Node { node_key; _ } -> node_key

    let make dependencies produce : ('context, 'output) t =
      Node
        {
          dependencies;
          produce;
          node_key = Hmap.Key.create ();
          output_key = Hmap.Key.create ();
        }
  end

  module Graph = struct
    include Types.Graph
    module Execution_cache = Types.Execution_cache

    let make (nodes : ('a, 'b) Node.t list) : t =
      let f (acc : Hmap.t) (Node n as next : (_, _) Node.t) =
        Hmap.add n.node_key next acc
      in
      let nodes_by_key = List.fold_left f Hmap.empty nodes in
      { nodes_by_key }

    let rec execute_node :
        type context deps output.
        context ->
        Execution_cache.t ->
        (context, deps, output) Dependencies.t ->
        deps ->
        output * Execution_cache.t =
     fun context cache deps f ->
      match deps with
      | [] -> (f, cache)
      | Node x :: xs -> (
          match Hmap.find x.output_key cache.outputs_by_key with
          (* If a node output has already been produced and is cached, use that value. *)
          | Some output -> execute_node context cache xs (f output)
          (* Otherwise, we need to compute the output for that node. *)
          | None ->
              let output, cache =
                execute_node context cache x.dependencies (x.produce context)
              in
              let cache : Execution_cache.t =
                {
                  outputs_by_key =
                    Hmap.add x.output_key output cache.outputs_by_key;
                }
              in
              execute_node context cache xs (f output))

    let execute (context : 'context) ({ nodes_by_key } : t)
        (output_key : ('context, 'output) Node.t Hmap.key) : 'output option =
      let cache : Execution_cache.t = { outputs_by_key = Hmap.empty } in
      match Hmap.find output_key nodes_by_key with
      | None -> None
      | Some (Node { produce; dependencies; _ }) ->
          let output, _ =
            execute_node context cache dependencies (produce context)
          in
          Some output
  end
end

module Producer = Make (Sync)

let int_node : (unit, int) Producer.Node.t =
  Producer.Node.make [] (fun () -> 123)

let mul_node : (unit, int) Producer.Node.t =
  Producer.Node.make [ int_node ] (fun () n -> n * 5)

let n : int =
  Option.get
  @@ Producer.Graph.execute ()
       (Producer.Graph.make [ int_node; mul_node ])
       (Producer.Node.node_key mul_node)
