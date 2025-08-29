tsort v2.1.
===========

tsort is a simple text file sort command line program. It takes as input a text file and produces as output another text file,
with its lines sorted, depending on the parameters specified by the user.

Usage:
  TSORT input-file output-file [sort-oder] [COLS:sortcolumns-list] [options]

  input-file        path to the sort input file
  output-file       path to the sort output file; if the path doesn't contain a directory, the input file directory is used; the output file may be identical to
                    the input file if -O is specified (you may use * instead of output-file if you want to override the input file)
  sort-oder         ASC (default) or DESC
  sortcolumns-list  sortcolumns, separated by commas (no spaces allowed)
    sortcolumns     startcolumn and endcolumn, separated by a hyphen
  options           (only considered with key-based sorting, except for -O)
                    -A   sort characters with accents by character-code (see below)
                    -C   consider letter case for sort
                    -D   remove lines with duplicate key values
                    -H:n consider first n lines as header
                    -O   allow overriding an existing output-file
                    -S   consider spaces for sort
                    -Y   consider symbols (hyphen, apostrophe) for sort

Files may be sorted on one or several keys, these keys being given as column start and end values. This does, of course, only work, if the file is organized in
columns, what means here, that a line contains several parts of data and these parts are always at the same position within the line. Also note, that the different
columns have to be separated by spaces. If there is no sortcolumns-list, the lines are simply alphabetically sorted on their content.

Here's an example of 2 lines of the data file blumen.txt, describing the flowers of my Blumenquiz application:

Frühlings-Krokus              -23       Krokus              -X   0306      Crocus vernus                 Frühlings-Safran
Gänseblümchen                 123                           W    0311      Bellis perennis               -Ausdauerndes Gänseblümchen, -Mehrjähriges Gänseblümchen

To sort this file first on the flowers' flowering month, and within this one, on the flowers' scientific name (and overriding the input-file content with the sorted
flower data), run the program as follows:
  tsort blumen.txt * cols:66-69,76-105 -O

Normally you don't want to make your sort case-dependent and if you sort names or similar, you normally don't want to consider spaces or symbols like hyphens and
apostrophes (sorting for example Gartenkürbis before Garten-Stiefmütterchen). tsort does this by default. If, however, you want to consider the case, spaces and/or
symbols, just add the corr. sort option parameter(s).

Some of your text files may start with one or several lines, describing what the file contains, where the data is from or how it is organized. To tell tsort, that
these lines aren't part of the data to be sorted, declare them as header with the -H parameter.

When you create large files, it may happen, that you include some of your data twice or several times. Unwanted duplicate data may be removed from the output file
with the -D parameter. Two lines are considered identical (duplicates), if they have the same sort keys (the rest of the line eventually having different content).
Attention: The duplicate check being done on the sort keys, lines with keys, differing by letters with accent and these letters being transformed to the same "basic
letter", will be removed from the output file, what may not always be wanted!

Non-ANSI characters (letters with accents) may be sorted by two different ways: 1. ignoring the accents and sorting, ordered by the "basic letter" (ex: sorting é and
è under e; ä under a or under ae), 2. sorting, ordered by their actual character code (normally placing the letters with accents further down than all "basic letters").
As usually a "basic letter" based sort is prefered, this is the default option of the tsort program, the transformations from letters with accent to simple letters
being described in the file accents.txt. The file, included in the download archive, contains only a part of these transformations. You can add those, that you need,
by simply editing the file. Also, if you prefer German umlauts to be sorted under the "basic letter" instead of under "basic letter" + e, just change the corresponding
lines in accents.txt.

                                                                                                                         tsort, v2.1, (c) allu, October 2020 - June 2021
