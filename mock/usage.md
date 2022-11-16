# Usage

This is the mock usage of our program. The mock usage instruction covers both the commandline backend usage as well as frontend user interface usage.

## Importing new pgn files

```shellscript
./chessable-ocaml-commandline.exe import -pgn data/CapablancaYaffe.pgn -repertoire repertoires/repertoire.rpt
```

The above code should read a pgn file, parse it and combine it with existing repertoire file (or create one if not exist). The `.rpt` serialization is yet to be determined but a possible candidates would be a recursive serialization of repertoire data types. Notice that is import command
only read all moves in a `.pgn` file into the repertoire but will not adding line information.

## Adding lines to repertoire

```shellscript
./chessable-ocaml-commandline.exe add-lines -line data/lines.line -repertoire repertoires/repertoire.rpt
```

This line of code will read a line file and add reviewable lines to the repertoire. If the move order denoted by the line is already in the repertoire, it only adds review information (will check if there are identical lines in the repertoire already). But if the line is not in the repertoire, the system will check whether this move order is valid and add valid lines to repertoire as well.

```shellscript
./chessable-ocaml-commandline.exe serve -repertoire repertoires/repertoires/repertoire.rpt [-p]
```

This will start a server with the repertoire specified. The server should provide reviewing functionality, line editing functionalities, browsing functionalities etc. (see server routes).