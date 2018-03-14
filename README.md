# Scripts

## word

A wrapper around `antiword` and `docx2txt` that pipes to less by default. Can optionally take an output file as an argument. Requires `bash`, `less`, `antiword`, `docx2txt`.

## tg-conv

A converter for TextGrid files (Praat) to xlsx files. Currently very naïve and converting only the textual info, not metadata.

## papis-rename

A script to automatically rename the folders in my papis directory based on YAML metadata.

## add-paper

A script to automate some of my workflow when adding papers to the papis library. It prompts the user for the author's name, year of publication, and document title. It then renames the original file, adds it to the papis library, appends the year to the `info.yaml` file (papis can't do that yet), and then deletes the original file.
