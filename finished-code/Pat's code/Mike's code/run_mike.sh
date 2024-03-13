#!/bin/bash

output="mike_output"
directory="/Users/prid013/Documents/henry-natasha/bdhs-bounds-2023/finished-code/prologFile/pancake3_new_prolog_files_f2e"
#output="mike_output.txt"
mike_file="/Users/prid013/Documents/henry-natasha/bdhs-bounds-2023/finished-code/FilesForPat/mike_file2.pl"

echo "here1"
echo  $directory
echo | ls $directory
for file in "$directory"/*; do
    echo "here2"
    echo $file
    ls $file
    echo -e "lbComputation(f2e_lower_bound_pair)." > $mike_file
    echo -e -n  "problem(\"" >> $mike_file
    echo -e -n "$file" >> $mike_file
    echo -e  "\")." >> $mike_file
    echo -e  "lb(Node1, Node2, LBVal) :- once(f2e_lower_bound_pair(Node1,Node2,LBVal))." >> $mike_file
    if [ -f "$file" ]; then
	real_file="'"$file"'"
	/Applications/SWI-Prolog.app/Contents/MacOS/swipl -g    "consult('/Users/prid013/Documents/henry-natasha/bdhs-bounds-2023/finished-code/FilesForPat/totalV05R2'),
    consult('/Users/prid013/Documents/henry-natasha/bdhs-bounds-2023/finished-code/FilesForPat/statsV01R2'),
    consult('/Users/prid013/Documents/henry-natasha/bdhs-bounds-2023/finished-code/FilesForPat/pancakeMods'),
    consult('/Users/prid013/Documents/henry-natasha/bdhs-bounds-2023/finished-code/FilesForPat/transIntoJSON'),
    consult($real_file), consult('/Users/prid013/Documents/henry-natasha/bdhs-bounds-2023/finished-code/FilesForPat/mike_file2.pl'),
    consult('/Users/prid013/Documents/henry-natasha/bdhs-bounds-2023/finished-code/FilesForPat/mike_file.pl'),halt." >> $output
	python3.7 code-mike-output-to-csv.py
    fi
#    python3.7 code-csv-to-graphs.py 
done



