open Deck
type player = {cards: deck; cash: int}

type returnplayer = {playerback: player; deckback: deck}
type deckpair = {maindeck: deck; hand: deck}

let createplayer (mdeck : deck) : returnplayer =
  let rec givecard (maindeck : deck) (drawncards: int) (returndeck : deck) : deckpair =
    if drawncards == 5 then {maindeck = maindeck; hand = returndeck} else
      (*
      let () = print_endline "in else" in
      *)
    let newcard = draw_random_card maindeck in
    let newcard_index = index_of_card newcard maindeck 0 in
    let deck_minus_newcard = delete_from_deck maindeck [] newcard_index 0 in
    givecard deck_minus_newcard (drawncards+1) (newcard :: returndeck) in
  let carddeckpair = givecard mdeck 0 [] in
  let playerscards = carddeckpair.hand in
  let maindeckback = carddeckpair.maindeck in
  let newplayer = {cards = playerscards; cash = 100} in
  {playerback = newplayer; deckback = maindeckback}
