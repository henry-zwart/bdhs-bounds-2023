#!/bin/bash

directory="pancake5_new_prolog_files_vidal"
output="final_pancake5_vidal_new_test_July13_catch_fixed.txt"


echo "here1"
echo -e -n "{\"problems_data\": [" >> $output
for file in "$directory"/*; do
    echo "here2"
    echo $file
    ls $file
    
  if [ -f "$file" ]; then
      echo "here3"
      echo -e -n "{\"file\":\""  >> $output
      echo -e -n "$file" >> $output
      echo -e -n  "\","  >> $output

      swipl -g "consult('prolog/final_new_auto.pl'), consult('$file'),   consult('mike_file.pl'),halt." >> $output
      echo -e -n "}},"  >> $output

      echo "here6"
  fi
  echo "Results stored in $output_file"
done






