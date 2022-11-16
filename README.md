# Chessable-OCaml

This is exactly the same text in the proposal submission.

## Project Description

In this course project, we tried to build a customizable spaced repetition chess repertoire reviewing platform in the spirit of [_Chessable_](https://www.chessable.com/). The goal is to allow users to build up their own repertoire and annotations and do spaced repetition over those lines.

## Related Works

The most similar publically available resource is [_Chessable_](https://www.chessable.com/). Chessable has content created by experienced (often titled) players that can be purchased and learned with spaced repetition. However, users are not allowed to create their private repertoire. A spaced repetition tool that allows customized repertoire is [_ChessHQ_](https://chesshq.com/), but they have very limited personalization functionalities and don't allow the users to annotate their moves. This project only aims to limit the scope to a locally hosted server-style platform, but at the same time optimize the customization ability of the repertoires being learned.

## Packages Being Used

The project should rely on usual package building tools, so `dune` should be used as in the assignment. Also, this project will rely on `yojson` to serialize internal states like board state on the server side, etc. For chess-related `.pgn` files parsing we might build upon existing parsers like `rgrinberg/ocaml-pgn`, but none of them seems to be well documented and comprehensive, as most of them are for personal usage and do not have complete parsing functionality that supports all sorts of annotations). As a web application, we may want to use `dream` as a web framework, and possibly to use mostly `ReScript` for interface programming. As deemed proper we may need to write external bindings for React packages like `react-chessaboard`.


## Packages Details

### `rgrinberg/ocaml-pgn`

This is deprecated as it uses a lot of deprecated packages (an implementation from 9 years ago).


## Implementation Order

1. Backend file processing, `.pgn` function parsing, import export.
2. Internal representation of repertoire and lines, the `.mli` specified types and functionalities.
3. A backend commandline that can process `.pgn` files and generate repertoire serializations.
4. User interface for input lines.
5. User interface for input moves and get feedbacks.
6. Server that respond to user review inputs etc.
7. Glue everything and produce a working system.
