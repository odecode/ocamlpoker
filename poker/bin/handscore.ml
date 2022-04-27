open Deck


let rec check_same_rank (hand: deck) (cardtocheck_index: int) (curcard_index: int) (samesofar: int) : int =
  let handlength = List.length hand in
  if curcard_index == handlength-1 then 
  let () = print_endline (string_of_int samesofar) in
  samesofar
  else
    let cardtocheck = List.nth hand cardtocheck_index in
    let curcard = List.nth hand curcard_index in
    if cardtocheck.rank == curcard.rank then check_same_rank hand (cardtocheck_index) (curcard_index+1) (samesofar+1)
    else check_same_rank hand (cardtocheck_index) (curcard_index+1) samesofar


(* let rec check_same_suit (hand: deck) (cardtocheck_index: int) (curcard_index: int) (samesofar: int) : int =
  let handlength = List.length hand in
  if curcard_index == handlength then samesofar
  else
    let cardtocheck = List.nth hand cardtocheck_index in
    let curcard = List.nth hand curcard_index in
    if cardtocheck.suit == curcard.suit then check_same_suit hand cardtocheck_index+1 curcard_index samesofar+1
    else check_same_suit hand cardtocheck_index+1 curcard_index samesofar *)




let has_fullhouse (hand: deck) : bool =
  let rec samecheck (handIn: deck) (cardtocheckindex : int) (returnlist : int list) : int list =
    if cardtocheckindex == 5 then returnlist else
    let samenumas = check_same_rank handIn cardtocheckindex 0 0 in
    samecheck handIn (cardtocheckindex+1) (samenumas :: returnlist) in
      
    let rec printsamecards (sc : int list) (cur:int) : unit =
      if cur == List.length sc then ()
      else
      let curint = List.nth sc cur in
      let curstr = string_of_int curint in
      let () = print_endline curstr in
      printsamecards sc (cur+1) in
  let samecards = samecheck hand 0 [] in
  
  let () = printsamecards samecards 0 in


  let existsTwoSame = List.exists (fun num -> num == 2) samecards in
  let existsThreeSame = List.exists (fun num -> num == 3) samecards in
  if existsTwoSame && existsThreeSame then true else false