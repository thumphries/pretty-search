# pretty-search
Explorations in treating pretty-printing as a search problem

Pretty-printers have a bunch of heuristics around whitespace and width.
These are usually burned into the library, perhaps parameterised by
ribbon width or a maximum document length.

What happens when we instead encode all possible choices in our printer,
then apply heuristics to guide the search?

Current answer: ┐(‘～` )┌

May eventually circle back into bidirectional parsing/printing.
