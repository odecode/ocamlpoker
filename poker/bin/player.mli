open Deck
type player = {cards: deck; mutable cash: int; bot: bool; aggression: float}

type returnplayer = {playerback: player; deckback: deck}

val createplayer : deck -> bool -> returnplayer

type returnbet = {potback: int; cashleft: int}

val bet_cash : int -> int -> int -> returnbet

val createAllPlayers : int -> int -> returnplayer list -> deck -> returnplayer list