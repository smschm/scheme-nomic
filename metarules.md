Scheme Nomic Metarules
===========================

The goal
---------

The goal of Scheme Nomic is to have the bot say "<your nickname> is the winner" or something to that effect.
How this goal is acheived is collectively up to you and the other players.

Be nice
---------

Many 'tricks' are obvious but count as cheating: one person could potentially represent many players, but this is highly discouraged.
Same with impersonating a player by using their nickname.

Any sizable cabal can execute arbitrary code. Using this to crash the bot or otherwise cause undesirable and clearly non-gameplay activity
is discouraged. It's not clever or well-executed 'hacking' to do so, and effectively, everybody loses.
Similarly, one could end the game by flipping over the table while playing Monopoly; this proves nothing but that the table-flipper has
working muscles.

Scheme Nomic Ordinaryrules
===========================

Playing using the base system
-----------------------------

Only a given set of nicknames are players at a given time. A player may submit a proposal by saying:

     !p <proposal>

where `<proposal>` is a sexp that will, if the proposal is enacted, be evaluated.
The bot will report the unique number of this proposal.

A player may _vote_ on an un-enacted proposal by saying:

     !v [y|n] <proposal number>

The bot will not respond, but the vote (should be) recorded.

If the number of yes votes meets or exceeds a quorum, which is the
(number of players + 1)/2, rounded up, one can _enact_ the proposal by saying:

     !e <proposal number>

The proposal is then passed to eval to be evaluated.
Errors in evaluation are caught, but not reported on; the bot will only notify that there is an error.
If there aren't enough yes votes to meet a quorum, this will be stated.