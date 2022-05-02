open Deck
open Handscore
type player = {cards: deck; mutable cash: int; bot: bool; aggression: float; id: int}
type cashrecord = {id: int; cash: int}
type returnplayer = {playerback: player; deckback: deck}
type deckpair = {maindeck: deck; hand: deck}

let createplayer (mdeck : deck) (which : int) (isbot : bool) (cashIn : int) : returnplayer =
  let rec givecard (maindeck : deck) (drawncards: int) (returndeck : deck) : deckpair =
    if drawncards == 5 then {maindeck = maindeck; hand = returndeck} else
      
    let newcard = draw_random_card maindeck in
    let newcard_index = index_of_card newcard maindeck 0 in
    let deck_minus_newcard = delete_from_deck2 maindeck newcard_index in
    givecard deck_minus_newcard (drawncards+1) (newcard :: returndeck) in
  
  let carddeckpair = givecard mdeck 0 [] in
  let playerscards = carddeckpair.hand in
  let maindeckback = carddeckpair.maindeck in
  let () = Random.self_init() in
  let aggroLevel = Random.float 0.99 in
  let newplayer = {cards = playerscards; cash = cashIn; bot = isbot; aggression=aggroLevel; id=which} in
  {playerback = newplayer; deckback = maindeckback}

let rec createAllPlayers (nplayers: int) (so_far : int) (players : returnplayer list) (deckIn: deck) : returnplayer list =
  if so_far == nplayers then players
  else if so_far == 0 then let recplayerdata = createplayer deckIn so_far false 100 in
    let newdeck = recplayerdata.deckback in 
    createAllPlayers nplayers (so_far+1) (recplayerdata :: players) newdeck
  else  let recplayerdata = createplayer deckIn so_far true 100 in
    let newdeck = recplayerdata.deckback in 
    createAllPlayers nplayers (so_far+1) (recplayerdata :: players) newdeck

let rec createAllPlayersWithCash (cashlist: cashrecord list) (nplayers: int) (so_far : int) (players : returnplayer list) (deckIn: deck) : returnplayer list =
  if so_far == nplayers then players
  else if so_far == 0 then 
    let playerlist = List.filter (fun x -> x.id==so_far) cashlist in
    let cashrec = List.nth playerlist 0 in
    let cash = cashrec.cash in
    let recplayerdata = createplayer deckIn so_far false cash in
    let newdeck = recplayerdata.deckback in 
    createAllPlayersWithCash cashlist nplayers (so_far+1) (recplayerdata :: players) newdeck
  else  
    let playerlist = List.filter (fun x -> x.id==so_far) cashlist in
    let cashrec = List.nth playerlist 0 in
    let cash = cashrec.cash in
    let recplayerdata = createplayer deckIn so_far true cash in
    let newdeck = recplayerdata.deckback in 
    createAllPlayersWithCash cashlist nplayers (so_far+1) (recplayerdata :: players) newdeck
    


exception TooLittleCashLeft of string


type returnbet = {potback: int; cashleft: int}


let bet_cash (amount : int) (pot : int) (cash : int) : returnbet =
  if cash-amount < 0 then raise (TooLittleCashLeft "Too little cash to bet that amount!") else
  {potback = pot+amount; cashleft = cash-amount}


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
  let largerThan = compare bet_sum (Float.of_int bot.cash) in

  if largerThan > 0 then bot.cash else 
    let final_sum = floor bet_sum in 
    
    Float.to_int final_sum