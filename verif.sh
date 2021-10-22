#!/bin/bash


RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
NC='\033[0m'

input="test_logs/tests"
login=$(cat student_log)
nbr_passed=0
nbr_failed=0
nbr_test=1
i=0
printf "\nTP2 CAML login : %s\n\n" $login
while IFS= read -r line
do
    firstCharacter=${line:0:1}
    if [ "$firstCharacter" == "/" ];then
        continue
    fi
    if [ "$firstCharacter" == "#" ]; then
        if [ $i -ne 0 ]; then
            printf "\nResult\n Number of tests : %i\n Number of ${GREEN}PASSED${NC}\
 tests %i \n Number of ${RED}FAILED${NC} tests : %i\n" $nbr_test $nbr_passed $nbr_failed
            printf "${YELLOW}Compilation Mark${NC} : %.4s\n" $(bc -l <<< "$nbr_passed / $nbr_test")
        fi
        printf "\n$line\n-----------------------\n"
        nbr_test=0
        nbr_passed=0
        nbr_failed=0
        continue
    fi
    i=$((i + 1))
    nbr_test=$((nbr_test + 1))
    expected=$(./correct $line 2>/dev/null)
    got=$(./output $line 2>/dev/null)
    if [ "$expected" != "$got" ]; then
        nbr_failed=$((nbr_failed + 1))
        printf "\nTest_"$nbr_test": $line${RED} Failed${NC} => \n"
        firstCharacter=${got:0:1}
        if [ "$firstCharacter" != "#" ]; then
            printf "\nGOT : \n-\n$got\n-\n  EXPECTED : \n-\n$expected\n-\n"
        else
            printf "Not_Done exception raised"
        fi
    else
        nbr_passed=$((nbr_passed + 1))
        printf "Test_"$nbr_passed" : $line ${GREEN}Passed ${NC}\n"
    fi
done < "$input"

