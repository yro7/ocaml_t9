open Encodage
open Chaines

(* Saisie des mots en mode T9 *)


(******************************************************************************)
(*                                                                            *)
(*      fonction d'encodage pour un caractère (mode intuitif)                 *)
(*                                                                            *)
(*   signature : encoder_lettre : encodage -> char -> int = <fun>             *)
(*                                                                            *)
(*   paramètre(s) : un encodage (liste associative) et un caractère           *)
(*   résultat     : la touche correspondante                                  *)
(*                                                                            *)
(******************************************************************************)
let encoder_lettre map c =
  match List.find_map (fun (touche, lettres) ->
    if List.mem c lettres then Some touche else None
  ) map with
  | Some t -> t
  | None -> failwith "encoder_lettre : caractère non trouvé dans l'encodage"


(* Tests unitaires *)

let%test _ = encoder_lettre t9_map 'a' = 2
let%test _ = encoder_lettre t9_map 'b' = 2
let%test _ = encoder_lettre t9_map 'd' = 3
let%test _ = encoder_lettre t9_map 'z' = 9

let%test _ = encoder_lettre stupide_map 'a' = 2
let%test _ = encoder_lettre stupide_map 'b' = 3

