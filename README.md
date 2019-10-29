# Scripts

## word

A wrapper around `antiword` and `docx2txt` that pipes to less by default. Can optionally take an output file as an argument. Requires `bash`, `less`, `antiword`, `docx2txt`.

## tg-conv

A converter for TextGrid files (Praat) to xlsx files. Currently very naïve and converting only the textual info, not metadata.

## papis-rename

A script to automatically rename the folders in my papis directory based on YAML metadata.

## add-paper

A script to automate some of my workflow when adding papers to the papis library. It prompts the user for the author's name, year of publication, and document title. It then renames the original file, adds it to the papis library, appends the year to the `info.yaml` file (papis can't do that yet), and then deletes the original file.

## hbar

A small Haskell script that checks the battery level and sends it to stdout. Meant to be used with a status bar.

## csv-praat

A Haskell script to convert CSV files in the format (begin-time, end-time, text) to Praat TextGrid files. I used this with [Parlatype](http://gkarsay.github.io/parlatype/), so the parser is aimed at Parlatype-style timestamps.

## csv-combinator

A Haskell script that combines CSV files with a specific content layout. The first two columns are the same, and act as a key. The rest of the colums are the value, and their number is not defined or limited. Keys can be repeated, in which case the different values are treated as a matrix (list of lists). The script finds the corresponding keys and combines any number of files, starting with a single file (the 'original'). The output is a merged file of all the inputs plus the original, and a diff file against the original for each input file.
