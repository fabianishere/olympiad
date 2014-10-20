#!/bin/sh
DIR=$(dirname $0)
echo "" > $DIR/results.txt
for a in 1 2 3 4 5 6 7 8 9 10
do
	for b in 1 2 3 4 5 6 7 8 9 10
	do
		for c in 1 2 3 4 5
		do
			for d in 1 2 3 4 5
			do
				echo "size= $a $b, steps=$c $d" >> $DIR/results.txt
				echo "$a $b\n$c $d" | python3 -mcProfile $DIR/chess.py >> $DIR/results.txt
			done
		done
	done
done
