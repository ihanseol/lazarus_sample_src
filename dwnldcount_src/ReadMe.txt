"dwnldcount" is a VERY SIMPLE command line program, that may be used to count the downloads from a website. Counting is done on a monthly base for
the year entered by the user and considering given types of files (files with a given extension), downloaded from one or two given webserver directories
(URIs). Which directories with which files to consider is defined in the configuration file dwnldcount.ini. The number of download counts to do (= number
of [downloadsN] sections in the .INI file) is defined by the value of the stats_download_counts key in the [stats] section.

Each of these download counts has its own section in the .INI file; keys are the following:
 - files_description = description, what data the files contain (e.g. my homeless people PDF articles);
 - files_url1 and files_url2 = the server directories, where the files are located (expressed as relative URL; thus, must begin with a slash;
   e.g. /sdf/articles/); if there is one single directory, let files_url2 blank;
 - files_ext = extension of the files to be considered for counting in this directory (must begin with a dot; e.g. .pdf);
 - files_server = number of the considered files actually available in the considered directory (not really useful, as varies from month to month;
   may be left blank); the value of this key is supposed to be the total number of files with given extension in given directory;
 - files_exclude = files not to be considered in counting (e.g. article00, this PDF not being an article, but a list of all articles). To specify
   several files to be excluded, use a semicolon as separation character.

The input for the program is taken from text files, one or several for each month, and containing the statistics of file downloads from your webserver.
Location, base name, possible name suffixes (and extension) of these statistics files are defined in the [stats] section of the .INI file. It is mandatory
that the base file name ends with a date, with the format "YYMM" or "YYYYMM". This base name may have a suffix after the date and being separated from the
date with one space. Allowed suffixes are defined by the key stats_files_suffixes in the [stats] section (as a list of items separated by semicolon). For
a given month, the program considers all files that have a filename including the corresponding date, as such or plus and any of the suffixes stated. Counting
is done from January to the current month, if the current year has been entered, to December, otherwise. If there is no file found for a month, the corresponding
counts in the program output will be 0.

The input text files must have a given format: each line has to contain:
 - a relative file URI (thus, begin with a slash);
 - one or more spaces (or a tab character, replaced by a space by the program)
 - the number of (monthly) downloads for this file;
 - the line may contain further data, separated from the download number by a space (or tab).

You may create these input files manually or writing a program to do it. On HelioHost Tommy, I created them myself by hand, based on the output of the statistics
web application awstats (available on the webserver of most webspace hosting providers): Just displaying the statistics for the different months, clicking the
"Downloads > Full list" link and then copy/paste the list data into a text file. On my HestiaCP-VPS, I copy/paste the download statistics directly from the monthly
awstats files.

The program output with the monthly download counts is a text file called downloadsYYYY.txt (saved to the directory, specified in the .INI file).
For each of the download counts to be done, there are two lines:
 - the first one shows the monthly download of different files, i.e. how many of the considered files actually present in the considered directory have been
   downloaded during a given month; e.g. in August, 181 Free Pascal executables of the 194 available;
 - the second one shows the total monthly file download, i.e. how may times the considered files in the considered directory have been downloaded during a
   given month (resp. last column: during the actual year so far); e.g. 466 times that a Free Pascal executable has been downloaded in August.

Please, note that the .INI file included in the download archive is the one, I actually use with my website and, of course, has to be modified by setting the
different keys to your own values. The "stats_files_suffixes=(http);(https);(vps)" setting allows to find files with names like: "awstats downloads - 202101.txt",
"awstats downloads - 202104 (http)", "awstats downloads - 202104 (https)" and "awstats downloads - 202109 (vps)"for example. The downloads2021.txt file is the
output, I got in January 2022 for the year 2021. It's just included as example, to show, what the program output looks like.

                                                                                                           dwnldcount, v2.0, © allu, September 2020 - January 2022.
