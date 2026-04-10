open Encodage
open Chaines


(* Saisie des mots sans le mode T9 *)
(* Pour marquer le passage d'un caractère à un autre, on utilise la touche 0 *)


(******************************************************************************)
(*                                                                            *)
(*      fonction d'encodage pour un caractère                                 *)
(*                                                                            *)
(*   signature : encoder_lettre : encodage -> char -> (int * int) = <fun>     *)
(*                                                                            *)
(*   paramètre(s) : un encodage (liste associative) et un caractère           *)
(*   résultat     : le couple (touche, nb_pressions)                          *)
(*                                                                            *)
(******************************************************************************)
let encoder_lettre map c =
  (* Fonction auxiliaire pour trouver l'index d'un caractère dans une liste *)
  let rec index i = function
    | [] -> None
    | x :: _ when x = c -> Some i
    | _ :: tl -> index (i + 1) tl
  in
  (* Utilisation de List.find_map pour trouver la touche et la position en un seul passage *)
  match List.find_map (fun (touche, chars) ->
    match index 1 chars with
    | Some pos -> Some (touche, pos)
    | None -> None
  ) map with
  | Some res -> res
  | None -> failwith "encoder_lettre : caractère non trouvé dans l'encodage"

(* Tests unitaires *)
let%test _ = encoder_lettre t9_map 'a' = (2, 1)
let%test _ = encoder_lettre t9_map 'b' = (2, 2)
let%test _ = encoder_lettre t9_map 'c' = (2, 3)
let%test _ = encoder_lettre t9_map 'd' = (3, 1)
let%test _ = encoder_lettre t9_map 'f' = (3, 3)
let%test _ = encoder_lettre t9_map 'p' = (7, 1)
let%test _ = encoder_lettre t9_map 's' = (7, 4)
let%test _ = encoder_lettre t9_map 'z' = (9, 4)

let%test _ = encoder_lettre stupide_map 'a' = (2, 1)
let%test _ = encoder_lettre stupide_map 'e' = (2, 2)
let%test _ = encoder_lettre stupide_map 'b' = (3, 1)
let%test _ = encoder_lettre stupide_map 'z' = (3, 20)
