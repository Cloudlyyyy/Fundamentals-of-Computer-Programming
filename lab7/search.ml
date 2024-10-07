(* search.ml: search strategies *)
(* Student name: Cloudly Ceen   *)
(* CMS cluster login name:      *)

module type Storage =
  sig
    type 'a t
    exception Empty

    val create : unit -> 'a t
    val push : 'a -> 'a t -> unit
    val pop : 'a t -> 'a
    val is_empty : 'a t -> bool
  end

module type Domain =
  sig
    type t
    val show : t -> string
    val is_solved : t -> bool
    val compare : t -> t -> int
    val next : t -> t list
  end

module Search (S : Storage) (D : Domain) =
  struct
    module DS = Set.Make(D)
    let search init =  
      let rec solve s v =
          if S.is_empty s then raise Not_found 
            else let hist = S.pop s in
              let h = List.hd hist in 
                if DS.mem h v then solve s v
                    else if D.is_solved h then hist 
                      else 
                        let new_visit = DS.add h v and
                        children = (D.next h) and 
                        gen new_board = S.push (new_board :: hist) s in
                        begin 
                          List.iter gen children;
                          solve s new_visit
                        end 
      in 
      let storage = S.create () in 
      let history = [init] in 
      let visited = DS.empty in
      begin 
        S.push history storage;
        solve storage visited
      end 

    let show_history hist =
      (String.concat "\n----\n\n" (List.map D.show (List.rev hist))) ^ "\n"
  end

