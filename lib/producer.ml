module type MONAD = sig
  type 'a t

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

module type MONAD2 = sig
  type ('a, 'b) t

  val return : 'a -> ('a, 'b) t
  val bind : ('a, 'b) t -> ('a -> ('c, 'b) t) -> ('c, 'b) t
end


exception Cyclic

module Make (M : MONAD) = struct
  let ( let* ) = M.bind
  let ( =<< ) fn x = M.bind x fn

  module Types = struct
    module rec Graph : sig
      type ('context, 'output) t = { execute : 'context -> 'output M.t }
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
            dependencies : ('context, 'deps, 'output M.t) Dependencies.t;
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

    let rec index : type context output. Hmap.t -> (context, output) t -> Hmap.t
        =
     fun hmap (Node { node_key; dependencies; _ } as node) ->
      let hmap = Hmap.add node_key node hmap in
      let rec index_dependencies :
          type c d o. Hmap.t -> (c, d, o) Dependencies.t -> Hmap.t =
       fun hmap -> function
        | [] -> hmap
        | x :: xs ->
            let hmap = index hmap x in
            index_dependencies hmap xs
      in
      index_dependencies hmap dependencies

    let node_key : ('context, 'output) t -> ('context, 'output) t Hmap.key =
      function
      | Node { node_key; _ } -> node_key

    let make (dependencies : ('context, 'deps, 'output M.t) Dependencies.t)
        (produce : 'context -> 'deps) : ('context, 'output) t =
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

    let rec execute_node :
        type context deps output.
        context ->
        Execution_cache.t ->
        (context, deps, output) Dependencies.t ->
        deps ->
        (output * Execution_cache.t) M.t =
     fun context cache deps f ->
      match deps with
      | [] -> M.return (f, cache)
      | Node x :: xs -> (
          match Hmap.find x.output_key cache.outputs_by_key with
          (* If a node output has already been produced and is cached, use that value. *)
          | Some output -> execute_node context cache xs (f output)
          (* Otherwise, we need to compute the output for that node. *)
          | None ->
              let* output, cache =
                execute_node context cache x.dependencies @@ x.produce context
              in
              let* output = output in
              let cache : Execution_cache.t =
                {
                  outputs_by_key =
                    Hmap.add x.output_key output cache.outputs_by_key;
                }
              in
              execute_node context cache xs (f output))

    let execute (context : 'context) (nodes_by_key : Hmap.t)
        (output_node : ('context, 'output) Node.t) : 'output M.t =
      let cache : Execution_cache.t = { outputs_by_key = Hmap.empty } in
      match Hmap.find (Node.node_key output_node) nodes_by_key with
      | None -> failwith "Output node not found in graph!"
      | Some (Node { produce; dependencies; _ }) ->
          fst =<< execute_node context cache dependencies @@ produce context

    let check_acyclic :
        type context output. ?seen:Hmap.t -> (context, output) Node.t -> unit =
     fun ?(seen : Hmap.t = Hmap.empty)
         (Node { node_key; dependencies; _ } as node) ->
      let () =
        if Option.is_some (Hmap.find node_key seen) then raise Cyclic
        else ()
      in
      let seen = Hmap.add node_key node seen in
      let rec check_dependences :
          type c d o. Hmap.t -> (c, d, o) Dependencies.t -> unit =
       fun seen dependencies ->
        match dependencies with
        | [] -> ()
        | (Node { node_key; dependencies; _ } as node) :: xs ->
            check_dependences seen xs;
            let () =
              if Option.is_some (Hmap.find node_key seen) then raise Cyclic
              else ()
            in
            let seen = Hmap.add node_key node seen in
            check_dependences seen dependencies
      in
      check_dependences seen dependencies

    let make (output : ('context, 'output) Node.t) : ('context, 'output) t =
      check_acyclic output;
      let nodes_by_key = Node.index Hmap.empty output in
      let execute' context = execute context nodes_by_key output in
      { execute = execute' }
  end
end

module Make2 (M : MONAD2) = struct
  let ( let* ) = M.bind
  let ( =<< ) fn x = M.bind x fn

  module Types = struct
    module rec Graph : sig
      type ('context, 'output, 'e) t = { execute : 'context -> ('output, 'e) M.t }
    end =
      Graph

    and Execution_cache : sig
      type t = { outputs_by_key : Hmap.t }
    end =
      Execution_cache

    and Dependencies : sig
      type (_, _, _, _) t =
        | [] : ('context, 'output, 'output, 'e) t
        | ( :: ) :
            ('context, 'a, 'e) Node.t * ('context, 'b, 'output, 'e) t
            -> ('context, 'a -> 'b, 'output, 'e) t
    end =
      Dependencies

    and Node : sig
      type ('context, 'output, 'e) t =
        | Node : {
            dependencies : ('context, 'deps, ('output, 'e) M.t, 'e) Dependencies.t;
            produce : 'context -> 'deps;
            output_key : 'output Hmap.key;
            node_key : ('context, 'output, 'e) t Hmap.key;
          }
            -> ('context, 'output, 'e) t
    end =
      Node
  end

  module Dependencies = struct
    include Types.Dependencies
  end

  module Node = struct
    include Types.Node
    module Dependencies = Types.Dependencies

    let rec index : type context output e. Hmap.t -> (context, output, e) t -> Hmap.t
        =
     fun hmap (Node { node_key; dependencies; _ } as node) ->
      let hmap = Hmap.add node_key node hmap in
      let rec index_dependencies :
          type c d o e. Hmap.t -> (c, d, o, e) Dependencies.t -> Hmap.t =
       fun hmap -> function
        | [] -> hmap
        | x :: xs ->
            let hmap = index hmap x in
            index_dependencies hmap xs
      in
      index_dependencies hmap dependencies

    let node_key : ('context, 'output, 'e) t -> ('context, 'output, 'e) t Hmap.key =
      function
      | Node { node_key; _ } -> node_key

    let make (dependencies : ('context, 'deps, ('output, 'e) M.t, 'e) Dependencies.t)
        (produce : 'context -> 'deps) : ('context, 'output, 'e) t =
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

    let rec execute_node :
        type context deps output e.
        context ->
        Execution_cache.t ->
        (context, deps, output, e) Dependencies.t ->
        deps ->
        (output * Execution_cache.t, e) M.t =
     fun context cache deps f ->
      match deps with
      | [] -> M.return (f, cache)
      | Node x :: xs -> (
          match Hmap.find x.output_key cache.outputs_by_key with
          (* If a node output has already been produced and is cached, use that value. *)
          | Some output -> execute_node context cache xs (f output)
          (* Otherwise, we need to compute the output for that node. *)
          | None ->
              let* output, cache =
                execute_node context cache x.dependencies @@ x.produce context
              in
              let* output = output in
              let cache : Execution_cache.t =
                {
                  outputs_by_key =
                    Hmap.add x.output_key output cache.outputs_by_key;
                }
              in
              execute_node context cache xs (f output))

    let execute (context : 'context) (nodes_by_key : Hmap.t)
        (output_node : ('context, 'output, 'e) Node.t) : ('output, 'e) M.t =
      let cache : Execution_cache.t = { outputs_by_key = Hmap.empty } in
      match Hmap.find (Node.node_key output_node) nodes_by_key with
      | None -> failwith "Output node not found in graph!"
      | Some (Node { produce; dependencies; _ }) ->
          fst =<< execute_node context cache dependencies @@ produce context

    let check_acyclic :
        type context output. ?seen:Hmap.t -> (context, output, 'e) Node.t -> unit =
     fun ?(seen : Hmap.t = Hmap.empty)
         (Node { node_key; dependencies; _ } as node) ->
      let () =
        if Option.is_some (Hmap.find node_key seen) then raise Cyclic
        else ()
      in
      let seen = Hmap.add node_key node seen in
      let rec check_dependences :
          type c d o e. Hmap.t -> (c, d, o, e) Dependencies.t -> unit =
       fun seen dependencies ->
        match dependencies with
        | [] -> ()
        | (Node { node_key; dependencies; _ } as node) :: xs ->
            check_dependences seen xs;
            let () =
              if Option.is_some (Hmap.find node_key seen) then raise Cyclic
              else ()
            in
            let seen = Hmap.add node_key node seen in
            check_dependences seen dependencies
      in
      check_dependences seen dependencies

    let make (output : ('context, 'output, 'e) Node.t) : ('context, 'output, 'e) t =
      check_acyclic output;
      let nodes_by_key = Node.index Hmap.empty output in
      let execute' context = execute context nodes_by_key output in
      { execute = execute' }
  end
end

module Sync : MONAD with type 'a t = 'a = struct
  type 'a t = 'a

  let return a = a
  let bind a fn = fn a
end

include Make (Sync)
