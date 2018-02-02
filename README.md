# ShallowBlue2

When you're playing chess, do you ever think to yourself, "Gol darn it! All these side effects I'm producing are giving me angst!"? If you do, then Shallowblue2 is just the game for you. Unlike the plain old object-oriented [Python version][shallow-blue] it's based on, ShallowBlue2 is written in 100% pure uncut 24 carat Haskell. That's right: 0 emission chess playing that's mathematically guaranteed to leave the world exactly the way it was when you started.

[shallow-blue]: https://github.com/tacitblue/ShallowBlue

## Features and Implementation

### Game Logic

This version of ShallowBlue employs an event-sourced design, whereby the basic unit of data is a ```Move``` (consisting of a game piece and a position for it to move to) and the underlying representation of the game is an ordered list of such ```Move```s. The current state of the game is not stored but rather derived from the game log. This data model has the advantage of allowing an easy way to evaluate special moves like castling and <i>en passant</i> capture (both of which are constrained by facts about previous moves) without having to resort to global state or some similar act of desperation.

### Display

Board rendering is accomplished via hscurses, a Haskell wrapper around the C ncurses library. As in any serious functional programming endeavor, the IO bottleneck is kept as tight as possible. All IO functions are bundled in the ```Display``` module which is only ever invoked when prompting a human player for a move.