#! /usr/bin/env bash

REPS=$1

echo READY

while true; do
    read -r WORD REPS 
    if [ "$WORD" == "START_BENCH" ]; then
        for (( i=0; i < ${REPS}; i++));
        do 
            NOP="";
#            echo iter $i
        done
        echo END_BENCH;
    elif [ "$WORD" == EXIT ]; then
        exit 0
    else
        echo "ERROR, expected 'START_BENCH', got: $WORD"
        exit -1
    fi
done
