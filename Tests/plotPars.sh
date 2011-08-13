#!/usr/bin/bash

evo=$1
cnt=$2
curmin=$(date +%Y%m%d%H%M)
newdir=collect$curmin
prog=/cygdrive/j/AbaAba/dist/build/PlotEvolve/PlotEvolve.exe

echo New directory $newdir for $evo
mkdir $newdir
cd $newdir

echo Collectig parameter data...
$prog "j:\\Learn\\$evo" $cnt

feats=$(head -1 results.txt | sed 's/^# //')

echo "set terminal jpeg size 3200, 1920" > plotme
# echo "set terminal pdf" > plotme
echo "set output 'params.jpg'" >> plotme
# echo "set output 'params.pdf'" >> plotme
echo "set multiplot layout 4, 5" >> plotme

i=1
for feat in $feats
do
	i=$(($i + 1))
	if [ $i -gt 2 ]
	then
		echo $feat
		# echo "set output '$feat.jpg'" >> plotme
		echo "plot 'results.txt' using 1:$i title '$feat'" >> plotme
	fi
done
echo "unset multiplot" >> plotme
echo "set output" >> plotme

echo Running gnuplot...
gnuplot plotme
echo Done.
