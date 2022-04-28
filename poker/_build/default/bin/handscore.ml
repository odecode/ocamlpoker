open Deck


let rec check_same_rank (hand: deck) (cardtocheck_index: int) (curcard_index: int) (samesofar: int)  : int =
 if curcard_index > 4 then samesofar else
  let curcard = List.nth hand curcard_index in
  let cardtocheck = List.nth hand cardtocheck_index in

  if curcard.rank == cardtocheck.rank then check_same_rank hand cardtocheck_index (curcard_index+1) (samesofar+1)
  else check_same_rank hand cardtocheck_index (curcard_index+1) samesofar


  let rec check_same_suit (hand: deck) (cardtocheck_index: int) (curcard_index: int) (samesofar: int)  : int =
  if curcard_index > 4 then samesofar else
   let curcard = List.nth hand curcard_index in
   let cardtocheck = List.nth hand cardtocheck_index in
 
   if curcard.suit == cardtocheck.suit then check_same_suit hand cardtocheck_index (curcard_index+1) (samesofar+1)
   else check_same_suit hand cardtocheck_index (curcard_index+1) samesofar
 


  let rec samecheck (func : deck -> int -> int -> int -> int) (handIn: deck) (cardtocheckindex : int) (returnlist : int list) : int list =
    if cardtocheckindex == 5 then returnlist else
    let samenumas = func handIn cardtocheckindex 0 0 in
    samecheck func handIn (cardtocheckindex+1) (samenumas :: returnlist)

  let rec printsamecards (sc : int list) (cur:int) : unit =
    if cur == List.length sc then ()
    else
    let curint = List.nth sc cur in
    let curstr = string_of_int curint in
    let () = print_endline curstr in 
    printsamecards sc (cur+1)


let has_flush (hand : deck) : bool =
  
  let samecards = samecheck check_same_suit hand 0 [] in

  (* let () = printsamecards samecards 0 in *)

  let existsFourSame = List.exists (fun num -> num >= 4) samecards in
  if existsFourSame then true else false


let has_fullhouse (hand: deck) : bool =
  
  let samecards = samecheck check_same_rank hand 0 [] in

  (* let () = printsamecards samecards 0 in *)

  let existsTwoSame = List.exists (fun num -> num == 2) samecards in
  let existsThreeSame = List.exists (fun num -> num == 3) samecards in
  if existsTwoSame && existsThreeSame then true else false

let has_pair (hand : deck) : bool =
  let samecards = samecheck check_same_rank hand 0 [] in

  let existsTwoSame = List.exists (fun num -> num >= 2) samecards in
  if existsTwoSame then true else false

let has_triplet (hand : deck) : bool =
  let samecards = samecheck check_same_rank hand 0 [] in

  let existsThreeSame = List.exists (fun num -> num >= 3) samecards in
  if existsThreeSame then true else false

let has_quads (hand : deck) : bool =
  let samecards = samecheck check_same_rank hand 0 [] in

  let existsFourSame = List.exists (fun num -> num >= 4) samecards in
  if existsFourSame then true else false

let compareCards (card1 : card) (card2 : card) : int =
  let c1rank = rank_to_int card1.rank in 
  let c2rank = rank_to_int card2.rank in
  
  if c1rank > c2rank then 

    let () = print_endline "True" in
    1 
else if
  c1rank < c2rank then 
    let () = print_endline "False" in
    -1 
else 
  let () = print_endline "Neutral" in
  0

let has_straight (hand : deck) : bool =
  let () = print_endline "Hand before sort" in
  
  let () = print_deck hand 0 in
  
  let hand_sorted = List.sort (fun x y -> compare (rank_to_int x.rank) (rank_to_int y.rank)) hand in
  
  let () = print_endline "Hand after sort" in

  let () = print_deck hand_sorted 0 in
   
  true