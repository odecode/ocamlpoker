# OCamlPoker

A terminal poker demo written in OCaml by Otto Westerlund. Play against computer-controlled opponents, place a bet on your five-card hand, and see who takes the round.

Built as a hands-on learning project, OCamlPoker explores card modeling, recursive game flow, mutable player state, and randomized bot decisions through a small, interactive game.

## At a glance

- One human player against computer-controlled opponents.
- Five-card hands dealt from a shared 52-card deck.
- A starting balance of 100 cash per player.
- Bot betting influenced by hand scores, available cash, and randomized aggression.
- A simple terminal interface with revealed hands after each round.
- Repeat rounds with session cash balances carried forward.

## Quick start

You need Git, an OCaml compiler, and **Dune 3.1 or newer**. The game uses the OCaml standard library with no external runtime library dependencies. These commands work in a Unix-like shell, including Linux through WSL.

With [opam](https://opam.ocaml.org/doc/Install.html) installed and initialized, and an OCaml switch selected:

```sh
opam install 'dune>=3.1'
eval "$(opam env)"

git clone https://github.com/odecode/ocamlpoker.git
cd ocamlpoker/poker

dune build
dune exec poker
```

Run Dune from the **`poker/` subdirectory**, which contains `dune-project`. You can also launch the game with `dune exec ./bin/main.exe`.

Build and single-round gameplay verified with **OCaml 4.11.1 and Dune 3.1.1**.

## How to play

1. Enter the total number of players, **including yourself**. Choose **2–10** for a game against bots.
2. Everyone starts with **100 cash**, and each player receives five cards.
3. The bots place their bets. The game then displays the pot, your cards, and your available cash.
4. Enter a **non-negative integer** bet. A bet above your available cash is reduced to your remaining balance.
5. The game compares hand scores, announces the winner, and reveals the bots' cards.
6. Enter **`1`** to play another round or **`0`** to quit. Play also ends when your cash reaches zero.

Each round uses a fresh deck and a single betting step. You place your bet after the bots and do not need to match their bets. Cash balances last for the current session.

### Try a scripted round

From `poker/`, run:

```sh
printf '2\n5\n0\n' | dune exec poker
```

This starts a game against one bot, bets 5, and quits after the hand. Cards, bot bets, and the outcome vary between runs.

## Explore the code

The project lives in `poker/`, with gameplay modules in `bin/`:

| File | Purpose |
| --- | --- |
| [`main.ml`](poker/bin/main.ml) | Terminal prompts and the round loop |
| [`deck.ml`](poker/bin/deck.ml) | Card types, deck creation, random drawing, and card display |
| [`player.ml`](poker/bin/player.ml) | Player state, dealing, cash balances, and bot betting |
| [`handscore.ml`](poker/bin/handscore.ml) | Hand classification and numeric scoring |
| [`gamehelper.ml`](poker/bin/gamehelper.ml) | Collecting bets, selecting a winner, and revealing hands |
| [`dune-project`](poker/dune-project) | Build configuration and package metadata |

The deck, player, and hand-scoring modules also have `.mli` interfaces describing their exposed types and functions. Dune generates `poker.opam` from the metadata in `dune-project`.

To explore the implementation, start with `main.ml` and follow the calls into the dealing, betting, and scoring modules. The scripted round above is a convenient way to try changes to the game flow.

## Acknowledgments

Parts of the deck implementation were inspired by [jakekara/ocaml-cardstuff](https://github.com/jakekara/ocaml-cardstuff/blob/master/cards.ml), which served as a starting point for learning OCaml. See the attribution in [`deck.ml`](poker/bin/deck.ml).
