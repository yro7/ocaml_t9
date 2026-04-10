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
  match List.find_map (fun (touch, chars) ->
    (* List.find_index commence à 0, on ajoute 1 pour le nombre d'appuis *)
    List.find_index (( = ) c) chars
    |> Option.map (fun idx -> (touch, idx + 1))
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
