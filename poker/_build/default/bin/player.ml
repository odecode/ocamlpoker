open Deck
type player = {cards: deck; mutable cash: int; bot: bool; aggression: float}

type returnplayer = {playerback: player; deckback: deck}
type deckpair = {maindeck: deck; hand: deck}

let createplayer (mdeck : deck) (isbot : bool) : returnplayer =
  let rec givecard (maindeck : deck) (drawncards: int) (returndeck : deck) : deckpair =
    if drawncards == 5 then {maindeck = maindeck; hand = returndeck} else
      (*
      let () = print_endline "in else" in
      *)
    let newcard = draw_random_card maindeck in
    let newcard_index = index_of_card newcard maindeck 0 in
    let deck_minus_newcard = delete_from_deck2 maindeck newcard_index in
    givecard deck_minus_newcard (drawncards+1) (newcard :: returndeck) in
  let carddeckpair = givecard mdeck 0 [] in
  let playerscards = carddeckpair.hand in
  let maindeckback = carddeckpair.maindeck in
  let () = Random.self_init() in
  let aggroLevel = Random.float 0.99 in
  let newplayer = {cards = playerscards; cash = 100; bot = isbot; aggression=aggroLevel} in
  {playerback = newplayer; deckback = maindeckback}

let rec createAllPlayers (nplayers: int) (so_far : int) (players : returnplayer list) (deckIn: deck) : returnplayer list =
  if so_far == nplayers then players
  else if nplayers == 0 then let recplayerdata = createplayer deckIn false in
    let newdeck = recplayerdata.deckback in 
    createAllPlayers nplayers (so_far+1) (recplayerdata :: players) newdeck
  else  let recplayerdata = createplayer deckIn true in
    let newdeck = recplayerdata.deckback in 
    createAllPlayers nplayers (so_far+1) (recplayerdata :: players) newdeck

exception TooLittleCashLeft of string


type returnbet = {potback: int; cashleft: int}


let bet_cash (amount : int) (pot : int) (cash : int) : returnbet =
  if cash-amount < 0 then raise (TooLittleCashLeft "Too little cash to bet that amount!") else
  {potback = pot+amount; cashleft = cash-amount}
