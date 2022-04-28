type suit = Diamond | Club | Heart | Spade
type rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | J | Q | K | ALOW | AHIGH

type card = {rank: rank; suit: suit}



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
    | ALOW -> "A"
    | AHIGH -> "A"

let rank_to_int (r : rank): int =
    match r with
    | ALOW -> 0
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
    | AHIGH -> 13

let int_to_rank (n : int): rank = 
    match n with
    | 0 -> ALOW
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
    | 13 -> AHIGH
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



let rec createdeck2 (partial_deck : deck) (cur_card: int) : deck =
    let cursuit = cur_card / 13 in
    let currank = cur_card mod 13 in
    if cur_card == 52 || cursuit > 3 then partial_deck
    else
        let ranktype = int_to_rank (currank) in 
        let suittype = int_to_suit (cursuit) in
        let newcard = {rank= ranktype; suit= suittype} in
        createdeck2 ( newcard
        :: partial_deck)
        (cur_card +1)


(* let rec createdeck (partial_deck : deck) (cur_card: int) : deck =
    let cursuit = cur_card / 13 in
    let currank = cur_card mod 13 in
    if cur_card == 52 || cursuit > 3 then partial_deck
    else
        createdeck ((int_to_rank (currank), int_to_suit (cursuit))
        :: partial_deck)
        (cur_card +1) *)

let card_to_string (cardIn : card) : string =
    Printf.sprintf "%s of %s"
        (rank_to_string(cardIn.rank))
        (suit_to_string(cardIn.suit))


let draw_card (cardset : deck) (whichCard : int) : card =
    List.nth cardset whichCard

let draw_random_card (cardset: deck) : card =
    let () = Random.self_init() in
    let randnum = Random.int (List.length cardset) in 
    draw_card cardset randnum



let delete_from_deck2 (olddeck : deck) (whichCard : int) : deck =
    let badcard = List.nth olddeck whichCard in
    
    let newdeck = List.filter (fun card -> card != badcard) olddeck in

    newdeck


let rec index_of_card (item : card) (cardlist : deck) (index : int) : int =
    (*
    let () = print_endline "in index of card" in
    *)
    match cardlist with
    | [] -> -1
    | first :: tail -> if first = item then index else index_of_card item tail (index+1)

let print_card (cardIn : card) : unit =
    let cardstring = card_to_string cardIn in
    let () = print_endline cardstring in
    ()

let rec createHand (decksofar: deck): deck =
    if List.length decksofar == 5 then
    let () = print_endline "Success in createHand" in    
    decksofar else
    let () = print_endline "Give rank" in
    let rankint = read_int () in
    let () = print_endline "Give suit" in
    let suitint = read_int () in
    let ranktype = int_to_rank rankint in
    let suittype = int_to_suit suitint in
    let newcard = {rank=ranktype; suit=suittype} in
    createHand (newcard :: decksofar)

let rec print_deck (d : deck) (curcard : int) : unit =
    if curcard == List.length d then () else
    let card = List.nth d curcard in
    let () = print_card card in
    print_deck d (curcard+1)
    
