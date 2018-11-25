open Printf;;

module Assignment3: Sig.Assignment3 = 
struct
    type term = C of string | V of string | F of string * (term list)
    type form = PRED of string * (term list)
                  | NOT of form
                  | AND of form * form
                  | OR of form * form
                  | FORALL of term * form (* This term should be a variable only*)
                  | EXISTS of term * form (* This term should be a variable only*)
    exception Not_wff of (term list * form list)
    exception Not_closed of term list
    exception DPLL_unsat of int

    (* maintaining two hashtables for function, and predicate *)
    let (f_hash : (string, int) Hashtbl.t) = Hashtbl.create 123456;;

    let (pred_hash : (string, int) Hashtbl.t) = Hashtbl.create 123456;;

    (* this list is used for maintaining the predicate and function multiple arity list *)
    let pred_list : form list = [];;
    let func_list : term list = [];;

    (* this code is to remove duplicates from the list *)
    let remove_elt e l =
      let rec go l acc = match l with
        | [] -> List.rev acc
        | x::xs when e = x -> go xs acc
        | x::xs -> go xs (x::acc)
      in go l [];;

    let remove_duplicates l =
      let rec go l acc = match l with
        | [] -> List.rev acc
        | x :: xs -> go (remove_elt x xs) (x::acc)
      in go l [];;

    (* this code is to determine the length of a list *)
    let rec len(x : term list) : int = 
      match x with
      |[] -> 0
      |x :: xs -> 1 + len(xs);;

    (* this code is to check the well formedness of a term *)
    let rec check_wff_term (x : term) =
      match x with
      |C(_) -> true
      |V(_) -> true
      |F(y, z) -> (try
          if((Hashtbl.find f_hash y) != len(z)) then false else check_wff_term_list(z);
        with Not_found -> ((Hashtbl.add f_hash y (len(z))); check_wff_term_list(z);))
    and check_wff_term_list (x : term list) =
      match x with
      |[] -> true
      |x :: xs -> if(check_wff_term(x)) then check_wff_term_list(xs) else false;;

    let rec wff (x : form) : bool = 
        match x with
        |PRED(y, z) -> 
          (try
            if((Hashtbl.find pred_hash y) != len(z)) then false else check_wff_term_list(z);
          with Not_found -> ((Hashtbl.add pred_hash y (len(z))); check_wff_term_list(z);))
        |NOT(y) -> wff(y);
        |AND(y1, y2) -> wff(y1) && wff(y2);
        |OR(y1, y2) -> wff(y1) || wff(y2);
        |FORALL(V(_), y) -> wff(y);
        |EXISTS(V(_), y) -> wff(y);
        |_ -> false;;

    let fv (x : form) : term list = [];;
    let closed (x : form) : bool = false;;
    let scnf (x : form) = x ;;
    let dpll x y : (term list * form list) = ([], []);;
    let sat x y : (bool * term list * form list) = (false, [], []);;

end;;

(* ------------------------------------------------------------------ RUNNING THE LOGIC ------------------------------------------------------------------ *)

let a : Assignment3.term = C("a")
let b : Assignment3.term = C("b")
let temp3 : Assignment3.form = PRED("p", [F("f", [a]); F("f", [a; b])] )
let temp2 : Assignment3.form = PRED("p", [F("f", [a]); F("f", [a])])
let temp1 : Assignment3.form = AND(PRED("p", [F("f", [a]); F("f", [a])]), NOT(PRED("p", [F("f", [a])])))
let answer : bool = (Assignment3.wff temp1);;
let () = if(answer) then print_string("VALID FORMULA. \n") else print_string("NOT A VALID FORMULA. \n");;