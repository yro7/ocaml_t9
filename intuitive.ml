open Encodage
open Chaines

(* Saisie des mots en mode T9 *)


(******************************************************************************)
(*                                                                            *)
(*      fonction d'encodage pour un caractère                                 *)
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


(******************************************************************************)
(*                                                                            *)
(*                        Type dictionnaire                                   *)
(*                                                                            *)
(******************************************************************************)
type dico = Noeud of (string list * (int * dico) list)

(******************************************************************************)
(*                                                                            *)
(*                       Dictionnaire vide                                    *)
(*                                                                            *)
(******************************************************************************)
let empty = Noeud ([], [])

(******************************************************************************)
(*                                                                            *)
(*                  Ajouter un mot au dictionnaire                            *)
(*                                                                            *)
(*   signature : ajouter : encodage -> dico -> string -> dico = <fun>         *)
(*                                                                            *)
(*   paramètre(s) : un encodage, un dictionnaire, un mot                      *)
(*   résultat     : le nouveau dictionnaire contenant le mot                  *)
(*                                                                            *)
(******************************************************************************)
let ajouter map dico mot =
  let touches = encoder_mot map mot in
  let rec aux (Noeud (mots, fils)) ts =
    match ts with
    | [] -> Noeud (mot :: mots, fils)
    | t :: reste ->
        let sous_dico = 
          match List.assoc_opt t fils with
          | Some sd -> sd
          | None -> empty
        in
        let nouveau_sous_dico = aux sous_dico reste in
        Noeud (mots, (t, nouveau_sous_dico) :: (List.remove_assoc t fils))
  in
  aux dico touches

(******************************************************************************)
(*                                                                            *)
(*                 Créer un dictionnaire depuis un fichier (4.3)              *)
(*                                                                            *)
(*   signature : creer_dico : encodage -> string -> dico = <fun>              *)
(*                                                                            *)
(*   paramètre(s) : un encodage, le chemin du fichier                         *)
(*   résultat     : le dictionnaire construit à partir du fichier             *)
(*                                                                            *)
(******************************************************************************)
let creer_dico map chemin =
  In_channel.with_open_text chemin (fun ic ->
    In_channel.fold_lines (fun d line -> 
      let trimmed = String.trim line in
      if trimmed = "" then d else ajouter map d trimmed
    ) empty ic
  )

(* Tests unitaires *)

let%test _ = empty = Noeud ([], [])

let d1 = ajouter t9_map empty "bon"
let%test _ = d1 = Noeud ([], [ (2, Noeud ([], [ (6, Noeud ([], [ (6, Noeud (["bon"], [])) ])) ])) ])

let d2 = ajouter t9_map d1 "bonne"
let%test _ = 
  match d2 with
  | Noeud (_, [ (2, Noeud (_, [ (6, Noeud (_, [ (6, Noeud (m3, f3)) ])) ])) ]) ->
      List.mem "bon" m3 && List.exists (fun (t, _) -> t = 6) f3
  | _ -> false

let d3 = ajouter t9_map empty "tendre"
let d4 = ajouter t9_map d3 "vendre"
(* "tendre" / "vendre" -> 8, 3, 6, 3, 7, 3 *)
let ts_tendre = encoder_mot t9_map "tendre"

let rec get_node (Noeud (mots, fils)) ts =
  match ts with
  | [] -> mots
  | t :: reste -> get_node (List.assoc t fils) reste

let%test _ = 
  let mots = get_node d4 ts_tendre in
  List.mem "tendre" mots && List.mem "vendre" mots

let%test "creer_dico integration" =
  let test_file = "test_dico_tmp.txt" in
  let oc = open_out test_file in
  output_string oc "bon\nbonne\nbonjour\nvendre\ntendre\n";
  close_out oc;
  let d_test = creer_dico t9_map test_file in
  Sys.remove test_file;
  let ts_bonjour = encoder_mot t9_map "bonjour" in
  let ts_tendre = encoder_mot t9_map "tendre" in
  List.mem "bonjour" (get_node d_test ts_bonjour) &&
  List.mem "tendre" (get_node d_test ts_tendre) &&
  List.mem "vendre" (get_node d_test ts_tendre)
let%test "prefix relationships" =
  let d = empty |> 
          (fun d -> ajouter t9_map d "a") |> 
          (fun d -> ajouter t9_map d "as") |> 
          (fun d -> ajouter t9_map d "ass") |> 
          (fun d -> ajouter t9_map d "assez") in
  let ts1 = encoder_mot t9_map "a" in
  let ts2 = encoder_mot t9_map "as" in
  let ts3 = encoder_mot t9_map "ass" in
  let ts4 = encoder_mot t9_map "assez" in
  List.mem "a" (get_node d ts1) &&
  List.mem "as" (get_node d ts2) &&
  List.mem "ass" (get_node d ts3) &&
  List.mem "assez" (get_node d ts4)

let%test "duplicate words" =
  let d = empty |> 
          (fun d -> ajouter t9_map d "test") |> 
          (fun d -> ajouter t9_map d "test") in
  let ts = encoder_mot t9_map "test" in
  let mots = get_node d ts in
  (* Depending on requirements, we might want to ensure no duplicates, 
     but current implementation adds them. Let's see if it works. *)
  List.length (List.filter (fun x -> x = "test") mots) >= 1

let%test "multiple collisions same node" =
  (* "tendre", "vendre", "vendue" all have the same T9 prefix path? 
     tendre: 8 3 6 3 7 3
     vendre: 8 3 6 3 7 3
     vendue: 8 3 6 3 8 3  -- wait, r(7) vs u(8). 
     Let's find words with same code.
     "bon" (2 6 6) and "amphi" (2 6 7 4 4) - no.
     "mots" (6 6 8 7) and "nous" (6 6 8 7) ?
     m - 6, o - 6, t - 8, s - 7
     n - 6, o - 6, u - 8, s - 7
     YES.
  *)
  let d = empty |> 
          (fun d -> ajouter t9_map d "mots") |> 
          (fun d -> ajouter t9_map d "nous") in
  let ts = encoder_mot t9_map "mots" in
  let mots = get_node d ts in
  List.mem "mots" mots && List.mem "nous" mots

let%test "creer_dico with malformed input" =
  let test_file = "test_malformed.txt" in
  let oc = open_out test_file in
  output_string oc "  \n  mots  \n\n  nous  \n  ";
  close_out oc;
  let d = creer_dico t9_map test_file in
  Sys.remove test_file;
  let ts1 = encoder_mot t9_map "mots" in
  let ts2 = encoder_mot t9_map "nous" in
  List.mem "mots" (get_node d ts1) && List.mem "nous" (get_node d ts2)

let%test "unsupported characters handling" =
  try
    let _ = encoder_mot t9_map "Hello!" in
    false (* should fail *)
  with Failure _ -> true

let%test "case sensitivity" =
  try
    let _ = encoder_mot t9_map "Bon" in
    false (* currently fails because 'B' is not in t9_map *)
  with Failure _ -> true

let%test "large dictionary simulation" =
  let words = ["apple"; "apply"; "ball"; "balloon"; "cat"; "dog"; "door"; "dot"] in
  let d = List.fold_left (ajouter t9_map) empty words in
  let verify word =
    let ts = encoder_mot t9_map word in
    List.mem word (get_node d ts)
  in
  List.for_all verify words
