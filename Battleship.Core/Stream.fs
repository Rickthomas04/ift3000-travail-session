namespace Battleship.Core

module Stream =

    type 'a Stream = Empty | Cons of 'a Lazy * 'a Stream Lazy

    (* ------- UTILITAIRES ------- *)
    (* --- Couvertes en classe --- *)

    let build (f: ('a -> 'a)) (x: 'a) : 'a Stream =
        let rec aux x' =
            Cons (lazy x', lazy (aux (f x')))
        in aux x

    let nth (stream: 'a Stream) (n: int) : 'a =
        let rec aux stream' n' =
            match stream' with
            | Empty -> failwith "Out of bounds."
            | Cons (x, r) ->
                if n' = 0 then x.Force()
                else aux (r.Force()) (n' - 1)
        in aux stream n

    let peak (n: int) (stream: 'a Stream) : 'a list =
        let rec aux n' stream' =
            match stream' with
            | Empty -> []
            | Cons (x, r) ->
                if n' = 0 then []
                else x.Force() :: (aux (n' - 1) (r.Force()))
        in aux n stream

    let map (f: ('a -> 'b)) (stream: 'a Stream) : 'b Stream =
        let rec aux stream' =
            match stream' with
            | Empty -> Empty
            | Cons (x, r) -> Cons (lazy (f (x.Force())), lazy (aux (r.Force())))
        in aux stream

    let filter (p: ('a -> bool)) (stream: 'a Stream) : 'a Stream =
        let rec aux stream' =
            match stream' with
            | Empty -> Empty
            | Cons (x, r) ->
                if p (x.Force()) then Cons (x, lazy (aux (r.Force())))
                else aux (r.Force())
        in aux stream

    let exists (p: ('a -> bool)) (stream: 'a Stream) : bool =
        let rec aux stream' =
            match stream' with
            | Empty -> false
            | Cons (x, r) -> p (x.Force()) || aux (r.Force())
        in aux stream

    let iter (f: ('a -> unit)) (stream: 'a Stream) : unit =
        let rec aux stream' =
            match stream' with
            | Empty -> ()
            | Cons (x, r) -> f (x.Force()); aux (r.Force())
        in aux stream

    (* ------- À COMPLÉTER ------- *)
    (* --- Nouvelles fonctions --- *)

    let cycleList (l: 'a list) : 'a Stream =
        match l with
        | [] -> Stream.Empty  
        | _ ->           
            let rec cycle listeUn listeDeux =
                match listeUn with
                | [] -> cycle listeDeux listeDeux  
                | head :: tail -> 
                    Stream.Cons (lazy head, lazy (cycle tail listeDeux))  
            cycle l l

    let head = function
        | Empty        -> failwith "Out of bounds." // pareil en haut
        | Cons (h, _)  -> h.Force()