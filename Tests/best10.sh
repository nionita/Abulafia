cd /j/Learn/$1/Games
if [ "$2" == old ]
then
	cat *.txt | sort -k4r | awk '{v=$3+$4; if (v>a[$2]) {a[$2]=v}} END {for(i in a){print i,a[i]}}' | sort -k2nr | head -12
else
	cat *-top.txt | sort -k4r | awk '{v=$3+$4; if (v>a[$2]) {a[$2]=v}} END {for(i in a){print i,a[i]}}' | sort -k2nr | head -12
fi
