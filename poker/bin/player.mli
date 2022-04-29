open Deck
type player = {cards: deck; mutable cash: int; bot: bool; aggression: float; id: int}
type cashrecord = {id: int; cash: int}
type returnplayer = {playerback: player; deckback: deck}

val createplayer : deck -> int -> bool -> int -> returnplayer

type returnbet = {potback: int; cashleft: int}

val bet_cash : int -> int -> int -> returnbet

val createAllPlayers : int -> int -> returnplayer list -> deck -> returnplayer list

val createAllPlayersWithCash : cashrecord list -> int -> int -> returnplayer list -> deck -> returnplayer list

val bot_bet : player -> int

val calc_bet : float -> int -> float -> float -> float