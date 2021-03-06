open Handscore
open Player

let calc_bet (base : float) (hand_score : int) (bluffFactor : float) (bluffComparisonFactor : float) : float =
  if bluffFactor < bluffComparisonFactor then
Float.mul (Float.add (base) (log(Float.of_int hand_score)))((Float.add 1.0 (Float.sub bluffComparisonFactor bluffFactor))) else
 Float.mul (base) (log (Float.of_int hand_score))

let bot_bet (bot: player) : int =
  let () = Random.self_init() in
  let score = score_hand bot.cards in
  let base_bet = Float.mul (Float.of_int bot.cash) bot.aggression in
  let bluffFactor = bot.aggression in
  let bluffCompareTo = Random.float 1.0 in
  
  let bet_sum = calc_bet base_bet score bluffFactor bluffCompareTo in
  (* let () = print_endline (string_of_int score) in
  let () = print_endline (Float.to_string base_bet) in
  let () = print_endline (Float.to_string bluffFactor) in
  let () = print_endline (Float.to_string bluffCompareTo) in
  let () = print_endline (Float.to_string bet_sum) in *)
  let largerThan = compare bet_sum (Float.of_int bot.cash) in

  if largerThan > 0 then bot.cash else 
    let final_sum = floor bet_sum in 
    
    Float.to_int final_sum
