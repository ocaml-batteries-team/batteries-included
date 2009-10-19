#!/usr/bin/perl -w
use strict;

my $filename = "x" x 120;
open FILE, ">", $filename or die "Couldn't open file: $!\n";
print FILE "foobie bletch\n";
close FILE;
