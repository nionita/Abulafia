#!/usr/bin/bash

cd /j/Learn/$1/Games
awk 'function pg(w,b,r) {
        print "[Event \"test\"]";
		print "[White \"" w "\"]";
		print "[Black \"" b "\"]";
		print "[Result \"" r "\"]";
		print "1. " r;
		print ""
	}
	function pag(a,b,w,l,r) {
		i=1;
		while(w-- > 0) { if (i) { pg(a,b,"1-0") } else { pg(b,a,"0-1") }; i=1-i }
		while(l-- > 0) { if (i) { pg(a,b,"0-1") } else { pg(b,a,"1-0") }; i=1-i }
		while(r-- > 0) { if (i) { pg(a,b,"1/2-1/2") } else { pg(b,a,"1/2-1/2") }; i=1-i }
	}
	{pag($1,$3,$4,$6,$8)}
    ' $1-*-res.txt > /J/Chess/LittleBlitzer-2.71/fake-$1.pgn
