open List
open Sets

(*********)
(* Types *)
(*********)

type ('q, 's) transition = 'q * 's option * 'q

type ('q, 's) nfa_t = {
  sigma: 's list;
  qs: 'q list;
  q0: 'q;
  fs: 'q list;
  delta: ('q, 's) transition list;
}

(***********)
(* Utility *)
(***********)

(* explode converts a string to a character list *)
let explode (s: string) : char list =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l)
  in
  exp (String.length s - 1) []

(****************)
(* Part 1: NFAs *)
(****************)

  let move (nfa: ('q,'s) nfa_t) (qs: 'q list) (s: 's option) : 'q list =
    let f a b = (match b with
                | (x,y,z) -> if List.mem x qs then (if y = s then z::a else a) else a)
                                                in  List.fold_left f [] nfa.delta

  let e_closure (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list =
    let f a b = (match b with
                | (x,y,z) ->  if List.mem x qs then (if y = None then 
                                                    (if (List.mem x a && List.mem z a) then a else (if List.mem x a then (if List.mem z a then a else z::a) else 
                                                                                                  (if  List.mem z a then (if List.mem x a then a else x::a)else x::z::a)))else (if List.mem z a then a else 
                                                                                                                              (if List.mem x a then a else x::a))) 
                                                                                                                              else (if List.mem z qs then (
                                                                                                                              if List.mem z a then a else z::a)else a))
                                                                                                                              in  List.fold_left f [] nfa.delta
  let rec accept_helper_part2  (a: ('q, 's) transition) (h: char) : bool =
    match a with
               | (x,y,z) -> 
               if y = (Some h) then true else (if y = None then true else false)

  let rec accept_helper (nfa: ('q, 's) transition list) (c: char list) (place: 'q list) (nfa_fake: ('q, 's) transition list): 'q list =
    match c with
    | h::t -> (match nfa_fake with
               | a::b -> if accept_helper_part2 a h then accept_helper nfa t (match a with
                                                                              | (x,y,z) -> [z]
                                                                              ) nfa else accept_helper nfa c (place @ place) b
               | [] -> place) 
    | [] -> place

  let accept (nfa: ('q,char) nfa_t) (s: string) : bool =
    if s = "" then false else (match (accept_helper nfa.delta (explode s) [nfa.q0] nfa.delta) with
                              | a::b -> if b = [] then(if List.mem a nfa.fs then true else false) else false
                              | [] -> false) 
                              

(*******************************)
(* Part 2: Subset Construction *)
(*******************************)
let new_states_helper (nfa: ('q, 's) nfa_t) (s: 's) (qs: 'q list) : 'q list = 
  let f a b = (match b with
                | (x,y,z) -> if List.mem x qs then (if y = (Some s) || y = None then z::a else a) else a)
                                                in  List.fold_left f [] nfa.delta

let new_states (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list = 
  let rec f a b = ( match a with
                      | h::t -> f t ((new_states_helper nfa h qs)::b)
                      | [] -> b) in f (nfa.sigma) []  


let new_trans (nfa: ('q,'s) nfa_t) (qs: 'q list) : ('q list, 's) transition list =
  failwith "unimplemented"

let new_finals (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  failwith "unimplemented"

let rec nfa_to_dfa_step (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t)
    (work: 'q list list) : ('q list, 's) nfa_t =
  failwith "unimplemented"

let nfa_to_dfa (nfa: ('q,'s) nfa_t) : ('q list, 's) nfa_t =
  failwith "unimplemented"
