#!/bin/perl -w

my %scc;
my $dir  = "dist/build/Abulafia";
my $file = "Abulafia_0_60_prof.prof";
my $path = "$dir/$file";
my $main = 0;

open IN, $path || die "Cannot open $path: $!\n";
while (my $line = <IN>) {
	next if $line =~ /^\s*$/;
	my @fields = split /\s+/, $line;
	if ($main) {
		$scc{$fields[1]} += $fields[4]	# we count only the entries
	} else {
		$main = 1 if $fields[0] eq "MAIN";
	}
}

for my $c (sort keys %scc) {
	printf "%-30s %10d\n", $c, $scc{$c};
}
