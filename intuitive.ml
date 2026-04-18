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


(******************************************************************************)
(*                                                                            *)
(*      fonction d'encodage pour un mot (mode intuitif)                      *)
(*                                                                            *)
(*   signature : encoder_mot : encodage -> string -> int list = <fun>         *)
(*                                                                            *)
(*   paramètre(s) : un encodage (liste associative) et un mot (chaîne)        *)
(*   résultat     : la suite de touches à presser (entiers)                   *)
(*                                                                            *)
(******************************************************************************)
let encoder_mot map mot =
  let chars = decompose_chaine mot in
  List.map (encoder_lettre map) chars


(* Tests unitaires *)

let%test _ = encoder_lettre t9_map 'a' = 2
let%test _ = encoder_lettre t9_map 'b' = 2
let%test _ = encoder_lettre t9_map 'd' = 3
let%test _ = encoder_lettre t9_map 'z' = 9

let%test _ = encoder_lettre stupide_map 'a' = 2
let%test _ = encoder_lettre stupide_map 'b' = 3

let%test _ = encoder_mot t9_map "bonjour" = [2; 6; 6; 5; 6; 8; 7]
let%test _ = encoder_mot t9_map "abc" = [2; 2; 2]
let%test _ = encoder_mot stupide_map "ae" = [2; 2]

(* Additional tests *)

(* Collision test: "tendre" and "vendre" have the same code in T9 *)
let%test _ = encoder_mot t9_map "tendre" = [8; 3; 6; 3; 7; 3]
let%test _ = encoder_mot t9_map "vendre" = [8; 3; 6; 3; 7; 3]

(* Single letter words *)
let%test _ = encoder_mot t9_map "p" = [7]
let%test _ = encoder_mot t9_map "s" = [7]
let%test _ = encoder_mot t9_map "z" = [9]

(* Empty string *)
let%test _ = encoder_mot t9_map "" = []

(* stupide_map tests (voyelles vs consonnes) *)
let%test _ = encoder_mot stupide_map "ocaml" = [2; 3; 2; 3; 3]
let%test _ = encoder_mot stupide_map "aoi" = [2; 2; 2]

