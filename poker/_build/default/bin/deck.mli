type suit

type rank

type card

type deck = card list

val  createdeck : card list -> int -> deck

val draw_card : deck -> int -> card

val draw_random_card : deck -> card

(* val delete_from_deck : deck -> deck -> int -> int -> deck *)

val delete_from_deck2 : deck -> int -> deck

val print_card : card -> unit

val index_of_card : card -> deck -> int -> int

val score_hand : deck -> int list