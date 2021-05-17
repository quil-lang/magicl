#!/bin/bash

declare -a scrub=("Right"
                  "Left"
                  "Epsilon"
                  "Safe minimum"
                 )

for w in "${scrub[@]}"
do
    grep -rl "\'$w\'" $1 | sort | uniq | xargs perl -pi -e "s/'$w'/'${w:0:1}'/g"
done
