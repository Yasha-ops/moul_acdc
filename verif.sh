#!/bin/bash


RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m'

input="test_logs/tests"
nbr_passed=0
nbr_failed=0
nbr_test=0

printf "\nTP2 CAML\n\n"
while IFS= read -r line
do
    firstCharacter=${line:0:1}
    if [ "$firstCharacter" == "#" ]; then
        printf "\n$line\n"
        nbr_test=0
        nbr_passed=0
        continue
    fi
    nbr_test=$((nbr_test + 1))
    expected=$(./correct $line 2>/dev/null)
    got=$(./output $line 2>/dev/null)
    if [ "$expected" != "$got" ]; then
        nbr_failed=$((nbr_failed + 1))
        printf "Test_"$nbr_test": $line${RED} Failed${NC} => "
        printf "GOT : $got EXPECTED : $expected DIFF :\n"
    else
        nbr_passed=$((nbr_passed + 1))
        printf "Test_"$nbr_passed" : $line ${GREEN}Passed ${NC}\n"
    fi
done < "$input"

