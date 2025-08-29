"dircmp" is a command line program that can be used to compare the content of two directories. It is intended to run on MS Windows. The program parameters
have to be specified on the command line. Usage:

dircmp  [<display>] <source-dir> <dest-dir> [<options>]
  <display>:
    --source  files existing in <source-dir> only
    --dest    files existing in <dest-dir> only
    --same    files with same name, size and date
    --diff    files with same name, but different size or date (default)
    --date    files with same name and size, but different date
    --size    files with same name and date, but different size
    --help    display program usage help
  <options>:
    -c             consider case for file name compare (default = no)
    -s             ignore seconds for file date compare (default = no)
    -o <filename>  output file (default = dircmp.txt)

The file list can be displayed in Command Prompt (instead of being written to a file by specifying: -o con:).

Subdirectories are listed between square brackets. As the program does not check their content, their size is also considered as being 0. That means that the
compare results shown are not necessary correct for directories:
  --same  the directories displayed have the same date, but may have a different content
  --diff  the directories displayed are different (different date), but some directories with same date, but different content may not be displayed
  --date  the directories displayed have a different date, but may also have a different content
  --size  this display will never include any directories

Comments and suggestions appreciated. Email: admin@streetinfo.lu
