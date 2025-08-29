tmerge v1.0.
============

tmerge is a simple text file merge command line program. It takes as input several text files and produces as output one single text file,
containing all the lines of all the files. The lines of the output file will be sorted, depending on the parameters specified by the user.

Usage:
  TMERGE file-1 file-2 [file-3 ...] OUT:output-file [sort-order] [COLS:sortcolumns-list] [options]

  file-n            paths to the input files to be merged; if a path doesn't contain a directory, the directory of file-1 is used
  output-file       path to the sort output file; if the path doesn't contain a directory, the directory of file-1 is used; the output file may be identical to
                    one of the input files if -O is specified
  sort-order         ASC (default) or DESC
  sortcolumns-list  sortcolumns, separated by commas (no spaces allowed)
    sortcolumns     startcolumn and endcolumn, separated by a hyphen
  options           (only considered with key-based sorting, except for -O)
                    -A   sort characters with accents by character-code (see below)
                    -C   consider letter case for sort
                    -D   remove lines with duplicate key values
                    -O   allow overriding (one of the input files or output file)
                    -S   consider spaces for sort
                    -Y   consider symbols (hyphen, apostrophe) for sort

Files may be sorted on one or several keys, these keys being given as column start and end values. This does, of course, only work, if the file is organized in
columns, what means here, that a line contains several parts of data and these parts are always at the same position within the line. Also note, that the different
columns have to be separated by spaces. Merging key-sorted files will of course only be possible, if they are all organized in an identical way. If there is no
sortcolumns-list, the lines are simply alphabetically sorted on their content.

Example: My Western Europe and North-West Europe Castle Quiz applications use data files, that are identically organized. Imagine, that I intend to write a new quiz
application with castles of both regions and that I want to create a single data file, sorted first on the country code and then on the castle name. Here some lines
of the two files castles1.txt and castles3.txt:

FRA 1 Abbaye du Mont-Saint-Michel
FRA - Abbaye Royale de Fontevraud
FRA 1 Château Azay-le-Rideau

ENG 2 Alnwick Castle
ENG 2 Arundel Castle
ENG 1 Bamburgh Castle

To merge the two files (supposed located at w:\test), sorted as said above, run the program as follows:
  tmerge w:\test\castles1.txt castles3.txt OUT:castles.txt cols:1-3,7-40

Normally you don't want to make your sort case-dependent and if you sort names or similar, you normally don't want to consider spaces or symbols like hyphens and
apostrophes. tmerge does this by default. If, however, you want to consider the case, spaces and/or symbols, just add the corr. sort option parameter(s).

When you create large files, it may happen, that you include some of your data twice or several times. Unwanted duplicate data may be removed from the output file
with the -D parameter. Two lines are considered identical (duplicates), if they have the same sort keys (the rest of the line eventually having different content).
Attention: The duplicate check being done on the sort keys, lines with keys, differing by letters with accent and these letters being transformed to the same "basic
letter", will be removed from the output file, what may not always be wanted!

Non-ANSI characters (letters with accents) may be sorted by two different ways: 1. ignoring the accents and sorting, ordered by the "basic letter" (ex: sorting é and
è under e; ä under a or under ae), 2. sorting, ordered by their actual character code (normally placing the letters with accents further down than all "basic letters").
As usually a "basic letter" based sort is preferred, this is the default option of the tmerge program, the transformations from letters with accent to simple letters
being described in the file accents.txt. The file, included in the download archive, contains only a part of these transformations. You can add those, that you need,
by simply editing the file. Also, if you prefer German umlauts to be sorted under the "basic letter" instead of under "basic letter" + e, just change the corresponding
lines in accents.txt.

tmerge is based on my tsort program, that may be used to sort one text file. The parameters of both programs are quite the same. Note, however, that tmerge does not
accept the -H parameter and that thus it is not possible to correctly merge files that contain some header information before the data.

                                                                                                                                     tmerge, v1.0, (c) allu, August 2021