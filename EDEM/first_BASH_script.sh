#!/bin/bash
a=$1
sum=0
for i in %(seq 1 100)
do
	sum=$(($sum+$a*$i))
done
echo $sum