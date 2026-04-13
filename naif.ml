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
    List.find_index (( = ) c) chars (* Trouve la position du caractère dans la map (0-indexé, d'où idx + 1 ci-dessous) *)
    |> Option.map (fun idx -> (touch, idx + 1)) (* On renvoie le couple associé (touche, nb_appuis) *)
  ) map with
  | Some res -> res
  | None -> failwith "encoder_lettre : caractère non trouvé dans l'encodage"

(******************************************************************************)
(*                                                                            *)
(*      fonction d'encodage pour un mot                                       *)
(*                                                                            *)
(*   signature : encoder_mot : encodage -> string -> int list = <fun>         *)
(*                                                                            *)
(*   paramètre(s) : un encodage (liste associative) et un mot (chaîne)        *)
(*   résultat     : la suite de touches à presser (entiers)                   *)
(*                                                                            *)
(******************************************************************************)
let encoder_mot map mot =
  let chars = decompose_chaine mot in
  (* concat_map map chaque caractère à sa séquence, puis concatène toutes séquences *)
  List.concat_map (fun c ->
    let (touche, nb_pressions) = encoder_lettre map c in
    List.init nb_pressions (fun _ -> touche) @ [0]
  ) chars



(******************************************************************************)
(*                                                                            *)
(*      fonction de décodage pour un caractère                                *)
(*                                                                            *)
(*   signature : decoder_lettre : encodage -> (int * int) -> char = <fun>     *)
(*                                                                            *)
(*   paramètre(s) : un encodage et un couple (touche, nb_pressions)           *)
(*   résultat     : le caractère correspondant                                *)
(*                                                                            *)
(******************************************************************************)
let decoder_lettre map (touche, nb_pressions) =
  match List.assoc_opt touche map with
  | None -> failwith "decoder_lettre : touche inexistante"
  | Some lettres -> 
      match List.nth_opt lettres (nb_pressions - 1) with
      | Some c -> c
      | None -> failwith "decoder_lettre : nombre de pressions invalide"


(* Tests unitaires *)

(* Fonction  encoder_lettre *)
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

(* Fonction encoder_mot *)

let%test _ = encoder_mot t9_map "abc" = [2; 0; 2; 2; 0; 2; 2; 2; 0]
let%test _ = encoder_mot t9_map "bonjour" = [2; 2; 0; 6; 6; 6; 0; 6; 6; 0; 5; 0; 6; 6; 6; 0; 8; 8; 0; 7; 7; 7; 0]
let%test _ = encoder_mot stupide_map "ae" = [2; 0; 2; 2; 0]

(* Fonction decoder_lettre *)
let%test _ = decoder_lettre t9_map (2, 1) = 'a'
let%test _ = decoder_lettre t9_map (2, 2) = 'b'
let%test _ = decoder_lettre t9_map (6, 3) = 'o'
let%test _ = decoder_lettre stupide_map (2, 5) = 'u'



