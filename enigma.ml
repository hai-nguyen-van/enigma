(** Rotor wiring tables *)
(** Rotors I, II, III extracted from Enigma I and IV, V from M3 Army *)
(*
let rotors =
  [| "EKMFLGDQVZNTOWYHXUSPAIBRCJ" ;
     "AJDKSIRUXBLHWTMCQGZNPYFVOE" ;
     "BDFHJLCPRTXVZNYEIWGAKMUSQO" ;
     "ESOVPZJAYQUIRHXLNFTGKDCMWB" ;
     "VZBRGITYUPSDNHLXAWMJQOFECK" ;
     "JPGVOUMFYQBENHZRDKASXLICTW" ;
     "NZJHGRCXMYSWBOUFAIVLPEKQDT" ;
     "FKQHTLXOCBJSPDZRAMEWNIUYGV" |]
*)
let enigmaI_rotor_I = "EKMFLGDQVZNTOWYHXUSPAIBRCJ"
and enigmaI_rotor_II = "AJDKSIRUXBLHWTMCQGZNPYFVOE"
and enigmaI_rotor_III = "BDFHJLCPRTXVZNYEIWGAKMUSQO"

let rot0 = [| enigmaI_rotor_I ; enigmaI_rotor_II ; enigmaI_rotor_III |]
type position = Left | Middle | Right

let ( @* ) = fun char (rotors, pos) ->
  let no_of_x = (int_of_char char) - (int_of_char 'A') in
  (rotors. (match pos with Left -> 0 | Middle -> 1 | Right -> 2)). [no_of_x];;

(** Wide-B reflector *)
let wide_b_refl = "YRUHQSLDPXNGOKMIEBFZCWVJAT"

let ( @$ ) = fun char reflector ->
  let no_of_x = (int_of_char char) - (int_of_char 'A') in
  reflector.[no_of_x];;

(** Plugboard *)
let pb0 =
  [('A', 'D') ; ('C', 'N') ; ('E', 'T') ; ('F', 'L') ;
   ('G', 'I') ; ('J', 'V') ; ('K', 'Z') ; ('P', 'U') ;
   ('Q', 'Y') ; ('W', 'X')]


let ( @! ) =
  let list_op l = List.map (fun (x, y) -> (y, x)) l in
  fun char plugboard -> try (List.assoc char plugboard) with
    | Not_found -> (try (List.assoc char (list_op plugboard)) with
	 | Not_found -> char
    );;

(** Rotor shifting *)
let sh0 = Array.map (fun rotor -> 0) rot0;;

let alpha = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
and beta = "BCDEFGHIJKLMNOPQRSTUVWXYZA"

exception StopShift;;

let ( @^ ) = fun rotors shift_index ->
  let shift s =
    for i = 0 to String.length s - 1 do
      s.[i] <- (beta.[i] >+ (alpha.[i] >-< s.[i]))
    done in
  try
    for i = 0 to Array.length rotors - 1 do
      match i with
	 | 0 -> 
	   shift_index.(0) <- (shift_index.(0) + 1) mod 26 ;
	   shift rotors.(0)
	 | _ ->
	     if shift_index.(i - 1) = 0
	     then
		shift_index.(i) <- (shift_index.(i) + 1) mod 26 ;
		shift rotors.(i)
	     else raise StopShift
    done
  with StopShift -> ()
;;

let shift s =
    for i = 0 to String.length s - 1 do
      s.[i] <- (be.[i] >+ (al.[i] >-< s.[i]))
    done
;;

beta.[9] >+ (alpha.[9] >-< 'Z');;
alpha.[9] >-< 'Z';;
beta.[9];;

let ( >+ ) = fun char n -> char_of_int ((((int_of_char char - int_of_char 'A') + n) mod 6) + int_of_char 'A');;
let ( >-< ) = fun c1 c2 -> int_of_char c1 - int_of_char c2
;;

(* HERE *)

let s0 = "EAFBDC";;
let alpha = "ABCDEF";;
let beta = "BCDEFA";;

s0;;

shift s0;;
shift s0;;
shift s0;;
shift s0;;

s0.[0] <- (be.[0] >+ (al.[0] >-< s0.[0]));;
s0;;

'A' @* (rot0, Left);;
'A' @$ wide_b_refl;;
'A' @! pb0;; 

(rot0, sh0);;
rot0 @^ sh0;;
(rot0, sh0);;

rot0 @^ sh0;;
rot0 @^ sh0;;
rot0 @^ sh0;;
rot0 @^ sh0;;
rot0 @^ sh0;;
rot0 @^ sh0;;
rot0 @^ sh0;;
rot0 @^ sh0;;
rot0 @^ sh0;;
rot0 @^ sh0;;
rot0 @^ sh0;;
rot0 @^ sh0;;
rot0 @^ sh0;;
rot0 @^ sh0;;
rot0 @^ sh0;;



let ( >+ ) = fun char n -> char_of_int ((((int_of_char char - int_of_char 'A') + n) mod 26) + int_of_char 'A');;
let ( >-< ) = fun c1 c2 -> int_of_char c1 - int_of_char c2
;;

let encrypt_symbol x (rotors, reflector, playboard, shift_index) =
  (((x @! playboard) @* (rotors, Right)) @* (rotors, Middle)) @* (rotors, Left) ...



exception Found of int;;
let rotor_assoc_inv x r =
  let no_of_x_in_r () =
    for i = 0 to (String.length r) - 1 do
      if r.[i] = x then raise (Found i)
    done ; '?' in
  try (no_of_x_in_r ()) with | Found i -> char_of_int (i + 65)
;;

let encrypt_symbol (x, conf) = 
  let plugboard_1 = sym_assoc x plugboard in
  
