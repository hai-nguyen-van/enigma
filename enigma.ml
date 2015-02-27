(**
   Wehrmacht Enigma I
   Functional-style execution of a historic encryption machine in OCaml

   Hai Nguyen Van
   Laboratoire de Recherche en Informatique, Université Paris-Sud
   
   Released under the terms of the CeCILL License
*)

let explode s =
  let clist = ref [] in
  let () = String.iter (fun c -> clist := !clist @ [c]) s in
  !clist;;

let alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

(* [x] @~ [n] is the modulo of the class of [x] in Z/[n]Z *)
let ( @~ ) = fun x n -> if x > -1 then x mod n else n - (abs x mod n);;

(* [c] >+ [n] is the character [c] shifted by [n] characters *)
let ( >+ ) = fun char n ->
  let alphabet_size = String.length alphabet in
  char_of_int ((((int_of_char char - int_of_char 'A') + n) @~ alphabet_size) + int_of_char 'A')

(* [c1] >-< [c2] is the signed distance between [c1] and [c2] *)
let ( >-< ) = fun c1 c2 -> int_of_char c1 - int_of_char c2

(** Rotor *)
type position = Left | Middle | Right
let ( @* ) = fun (rotors, pos) char ->
  let no_of_x = (int_of_char char) - (int_of_char 'A') in
  (rotors. (match pos with Left -> 0 | Middle -> 1 | Right -> 2)). [no_of_x];;

(** Reflector *)
let ( @$ ) = fun reflector char ->
  let no_of_x = (int_of_char char) - (int_of_char 'A') in
  reflector.[no_of_x];;

(** Plugboard *)
let ( @% ) =
  let list_op l = List.map (fun (x, y) -> (y, x)) l in
  fun plugboard char -> try (List.assoc char plugboard) with
    | Not_found -> (try (List.assoc char (list_op plugboard)) with
	 | Not_found -> char
    );;

(** Rotor Turnover *)
exception StopTurn;;

(* Rotor one step *)
let ( @^ ) = fun _ s ->
  let shifts = List.map2 (fun x y -> x >-< y) (explode s) (explode alphabet) in
  let shifts_turned = match shifts with
    | h :: l -> l @ [h]
    | _ -> assert false in
  let new_rotor = List.map2 (fun x y -> x >+ y) (explode alphabet) shifts_turned in
  for i = 0 to List.length new_rotor - 1 do
    s.[i] <- List.nth new_rotor i
  done
;;

(* Rotors turnover by lexicographic enumeration *)
(* BUG: use turnover positiions RFW instead of simply counting rotor steps *)
let ( @^^^ ) = fun (rotors, shift_index) x ->
  let alphabet_size = String.length alphabet in
  try
    begin
      for i = 0 to Array.length rotors - 1 do
	 match i with
	   | 0 -> 
	     shift_index.(0) <- (shift_index.(0) + 1) mod alphabet_size ;
	     () @^ rotors.(0)
	   | _ ->
	     if shift_index.(i - 1) = 0
	     then
		begin
		  shift_index.(i) <- (shift_index.(i) + 1) mod alphabet_size ;
 		  () @^ rotors.(i)
		end
	     else raise StopTurn
      done ;
      x
    end
  with StopTurn -> x
;;


(** Main encryption function *)
type symbol = char
type private_key = string array * string * (char * char) list * int array

(* Here, the input [x] comes at the "right-most" part of the term *)
let encrypt_symbol
    (x : symbol)
    ((rotors, reflector, plugboard, shift_index) : private_key)
    : symbol =
  plugboard @% (
    (rotors, shift_index) @^^^ (
      (rotors, Right) @* (
	 (rotors, Middle) @* (
	   (rotors, Left) @* (
	     reflector @$ (
		(rotors, Left) @* (
		  (rotors, Middle) @* (
		    (rotors, Right) @* (
		      plugboard @% 
			 x)))))))));;
;;

(** An example *)

(* Rotor wiring tables extracted from Enigma I machines *)
let enigmaI_rotor_I = "EKMFLGDQVZNTOWYHXUSPAIBRCJ"
and enigmaI_rotor_II = "AJDKSIRUXBLHWTMCQGZNPYFVOE"
and enigmaI_rotor_III = "BDFHJLCPRTXVZNYEIWGAKMUSQO"

let rot0 = [| enigmaI_rotor_I ; enigmaI_rotor_II ; enigmaI_rotor_III |];;
let pb0 =
  [('A', 'D') ; ('C', 'N') ; ('E', 'T') ; ('F', 'L') ;
   ('G', 'I') ; ('J', 'V') ; ('K', 'Z') ; ('P', 'U') ;
   ('Q', 'Y') ; ('W', 'X')];;
let wide_b_refl = "YRUHQSLDPXNGOKMIEBFZCWVJAT";;
let sh0 = Array.map (fun rotor -> 0) rot0;;

(* Entry-point *)
encrypt_symbol 'A' (rot0, wide_b_refl, pb0, sh0);;
