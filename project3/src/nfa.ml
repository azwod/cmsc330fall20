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

  let rec path(nfa: ('q, char) nfa_t) (s: char) (q: 'q * bool) : 'q * bool = 
     let f a b = (match b with
                | (x,y,z) -> if y = (Some s) then (z,true) else (if y = None then (z,true) else (x,false)))
                                                in  List.fold_left f q nfa.delta

  (*let rec accept_helper (nfa: ('q, char) nfa_t) (s: char list) (curr: 'q) : bool = 
    match s with
    | h::t -> if (match path nfa h (curr,true) with
                  | (a,b) -> b) then accept_helper nfa t (match path nfa h (curr,true) with
                                                          | (a,b) -> a) else false 
    | [] -> List.mem curr nfa.fs

  let accept (nfa: ('q,char) nfa_t) (s: string) : bool =
    accept_helper nfa (explode s) nfa.q0*)
  
  let rec accept_help nfa lis s= 
    match s with
    | h::t -> accept_help nfa (move nfa lis (Some h)) t
    | [] -> lis

  let accept (nfa: ('q,char) nfa_t) (s: string) : bool =
    List.mem (accept_help nfa (nfa_to_dfa nfa).q0 (explode s)) (nfa_to_dfa nfa).fs


                              

(*******************************)
(* Part 2: Subset Construction *)
(*******************************)

let rec loop (nfa: ('q, 's) nfa_t) (s: 's) (l: 'q list) (qs: 'q list) : 'q list = 
     let f a b = (match b with
                | (x,y,z) -> if (y = (Some s)) && (List.mem x qs) then (if List.mem z a then a else (loop nfa s (z::a) qs)) else (if y = None && List.mem x a then (if List.mem z a then a else (loop nfa s (z::a) qs) ) else a))
                                                in  List.fold_left f l nfa.delta


let new_states_helper (nfa: ('q, 's) nfa_t) (s: 's) (qs: 'q list) : 'q list = 
  let f a b = (match b with
                | (x,y,z) -> if List.mem x qs then (if y = (Some s) then (if List.mem z a then a else(if List.mem z qs then loop nfa s (z::a) qs else z::a)) else a) else (if y = None && (List.mem x qs)then(if List.mem z a then a else z::a)else loop nfa s a qs))
                                                in  List.fold_left f [] nfa.delta

let new_states (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list = 
  let rec f a b = ( match a with
                      | h::t -> f t ((new_states_helper nfa h qs)::b)
                      | [] -> b) in f (nfa.sigma) []  

let new_trans_helper (nfa: ('q, 's) nfa_t) (s: 's) (qs: 'q list) : ('q list, 's) transition =
  (qs, Some s, (new_states_helper nfa s qs))

let new_trans (nfa: ('q,'s) nfa_t) (qs: 'q list) : ('q list, 's) transition list =
    let rec f a b = ( match a with
                      | h::t -> f t ((new_trans_helper nfa h qs)::b)
                      | [] -> b) in f (nfa.sigma) []

let new_finals (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
   let rec f a b = ( match a with
                      | h::t -> if List.mem h qs then qs::b else f t b
                      | [] -> []) in f nfa.fs []  

let rec nfa_to_dfa_step (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t)
    (work: 'q list list) : ('q list, 's) nfa_t =
  failwith "unimplemented"

let rec nfa_to_dfa_help nfa dfa work taken = 
  match work with
  | h::t -> if ((List.mem h taken)=false) then nfa_to_dfa_help nfa {sigma=dfa.sigma; qs=union (filter (fun x -> x <> []) (new_states nfa h)) dfa.qs; q0 = dfa.q0; fs=dfa.fs; delta= union (filter (fun (v,p,o)-> o <> []) (new_trans nfa h)) dfa.delta } (union (filter (fun x -> x <> []) (new_states nfa h)) work) (union [h] taken) else nfa_to_dfa_help nfa dfa t taken
  | [] -> {sigma=dfa.sigma; qs=dfa.qs; q0=dfa.q0; fs= List.fold_left (fun a x -> union (new_finals nfa x) a) [] dfa.qs; delta=dfa.delta}

let nfa_to_dfa (nfa: ('q,'s) nfa_t) : ('q list, 's) nfa_t =
  nfa_to_dfa_help nfa {sigma=nfa.sigma; qs= [(e_closure nfa [nfa.q0])]; q0=(e_closure nfa [nfa.q0]); fs=[]; delta=[] } [e_closure nfa [nfa.q0]] []
