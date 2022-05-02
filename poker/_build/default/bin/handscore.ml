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

let has_flush (hand : deck) : bool =
  
  let samecards = samecheck check_same_suit hand 0 [] in

  let existsFourSame = List.exists (fun num -> num >= 4) samecards in
  if existsFourSame then true else false


let has_fullhouse (hand: deck) : bool =
  
  let samecards = samecheck check_same_rank hand 0 [] in

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


let has_straight (hand : deck) : bool =

  let newace = {rank = (int_to_rank 13); suit = (int_to_suit 0)} in

  let copyhand = newace :: (List.filter (fun card -> (rank_to_int card.rank) == 0) hand ) in  
  
  let hand_sorted = List.sort (fun x y -> compare (rank_to_int x.rank) (rank_to_int y.rank)) hand in
  
  let copyhand_sorted = List.sort(fun x y -> compare (rank_to_int x.rank) (rank_to_int y.rank)) copyhand in
 

  let rec check_next_bigger (handIn : deck) (anslist : bool list) (curcard_index : int) : bool list =
    if curcard_index == (List.length handIn)-1 then anslist else
    let curcard = List.nth handIn curcard_index in
    let nextcard = List.nth handIn (curcard_index+1) in
    let cur_rank_int = rank_to_int curcard.rank in
    let next_rank_int = rank_to_int nextcard.rank in
    if cur_rank_int == next_rank_int-1 then check_next_bigger handIn (true :: anslist) (curcard_index+1)
    else check_next_bigger handIn (false :: anslist) (curcard_index+1) in
  
    let boollist1 = check_next_bigger hand_sorted [] 0 in
    let boollist2 = check_next_bigger copyhand_sorted [] 0 in
    let boollist1result = List.exists (fun b -> b == false) boollist1 in
    let boollist2result = List.exists (fun b -> b == false) boollist2 in

    if boollist1result || (boollist2result) then false else true

  let highest_rank (hand : deck) : card = 
    let hand_sorted = List.sort (fun x y -> compare (rank_to_int x.rank) (rank_to_int y.rank)) hand in
    let card = List.nth hand_sorted 4 in
    card

  let score_hand (hand : deck) : int =
    let straight = has_straight hand in
    let flush = has_flush hand in
    let highest_card = highest_rank hand in
    let fullhouse = has_fullhouse hand in
    let quads = has_quads hand in
    let triplets = has_triplet hand in
    let pair = has_pair hand in
    if ((rank_to_int highest_card.rank) == 12  || (rank_to_int highest_card.rank) == 13) && straight && flush then 60
    else if straight && flush then 55
    else if quads then 50
    else if fullhouse then 40
    else if flush then 35
    else if straight then 30
    else if triplets then 25
    else if pair then 20
    else rank_to_int highest_card.rank
