type suit = Diamond | Club | Heart | Spade
type rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | J | Q | K | A

type card = rank * suit

exception InvalidCard

let rank_to_string (r : rank): string =
    match r with
    | Two -> "2"
    | Three -> "3"
    | Four -> "4"
    | Five -> "5"
    | Six -> "6"
    | Seven -> "7"
    | Eight -> "8"
    | Nine -> "9"
    | Ten -> "10"
    | J -> "J"
    | Q -> "Q"
    | K -> "K"
    | A -> "A"
(*
let rank_to_int (r : rank): int =
    match r with
    | A -> 0
    | Two -> 1
    | Three -> 2
    | Four -> 3
    | Five -> 4
    | Six -> 5
    | Seven -> 6
    | Eight -> 7
    | Nine -> 8
    | Ten -> 9
    | J -> 10
    | Q -> 11
    | K -> 12
    
*)
let int_to_rank (n : int): rank = 
    match n with
    | 0 -> A
    | 1 -> Two
    | 2 -> Three
    | 3 -> Four
    | 4 -> Five
    | 5 -> Six
    | 6 -> Seven
    | 7 -> Eight
    | 8 -> Nine
    | 9 -> Ten
    | 10 -> J
    | 11 -> Q
    | 12 -> K
    | _ -> raise InvalidCard


(*
   
let suit_to_int (s : suit) : int =
    match s with
    | Diamond -> 0
    | Club -> 1
    | Heart -> 2
    | Spade -> 3    
*)
let int_to_suit (n : int) : suit =
    match n with
    | 0 -> Diamond
    | 1 -> Club
    | 2 -> Heart
    | 3 -> Spade
    | _ -> raise InvalidCard

let suit_to_string (s : suit) : string =
    match s with
    | Diamond -> "Diamond"
    | Club -> "Club"
    | Heart -> "Heart"
    | Spade -> "Spade"


type deck = card list

let rec createdeck (partial_deck : deck) (cur_card: int) : deck =
    let cursuit = cur_card / 13 in
    let currank = cur_card mod 13 in
    if cur_card == 52 || cursuit > 3 then partial_deck
    else
        createdeck ((int_to_rank (currank), int_to_suit (cursuit))
        :: partial_deck)
        (cur_card +1)

let card_to_string ((rank,suit) : card) : string =
    Printf.sprintf "%s of %s"
        (rank_to_string(rank))
        (suit_to_string(suit))


let draw_card (cardset : deck) (whichCard : int) : card =
    List.nth cardset whichCard

let draw_random_card (cardset: deck) : card =
    let () = Random.self_init() in
    let randnum = Random.int (List.length cardset) in 
    draw_card cardset randnum

(*
   
let rec print_deck (decktocheck : deck) (cur_card_nr) : unit =
    let curcard = List.nth decktocheck cur_card_nr in
    let cardstr = card_to_string curcard in
    let () = print_endline cardstr in
    if cur_card_nr < 52 then print_deck(decktocheck) (cur_card_nr+1)
    else let () = print_endline "done" in
    ()
    
*)
let rec delete_from_deck (cardset : deck) (newdeck : deck) (whichCard : int) (curcard : int) : deck =
    (*
    let () = print_endline "in delete_from_deck" in
    *)
    let thiscard = draw_card cardset curcard in
    
    if (List.length newdeck) == (List.length cardset)-1 then newdeck
    else if curcard != whichCard then delete_from_deck cardset (thiscard :: newdeck) (whichCard) (curcard)
    else
        delete_from_deck cardset newdeck whichCard (curcard+1)

(*
   

let printcard (cardset : deck) (whichCard : int) : string =
    let mycard = List.nth cardset whichCard in
    let cardstr = card_to_string mycard in
    cardstr
    
*)

let rec index_of_card (item : card) (cardlist : deck) (index : int) : int =
    (*
    let () = print_endline "in index of card" in
    *)
    match cardlist with
    | [] -> 0
    | first :: tail -> if first = item then index else index_of_card item tail (index+1)

let print_card (cardIn : card) : unit =
    let cardstring = card_to_string cardIn in
    let () = print_endline cardstring in
    ()