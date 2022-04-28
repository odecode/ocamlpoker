type suit

type rank

type card = {rank: rank; suit: suit}

type deck = card list

(* val  createdeck : card list -> int -> deck *)
val  createdeck2 : card list -> int -> deck

val draw_card : deck -> int -> card

val draw_random_card : deck -> card

(* val delete_from_deck : deck -> deck -> int -> int -> deck *)

val delete_from_deck2 : deck -> int -> deck

val print_card : card -> unit

val index_of_card : card -> deck -> int -> int

val createHand : deck -> deck

val print_deck : deck -> int -> unit

val rank_to_int : rank -> int

val int_to_rank : int -> rank

val int_to_suit : int -> suit

val suit_to_int : suit -> int

(* val score_hand : deck -> int *)