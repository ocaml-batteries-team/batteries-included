#!/bin/env perl

use strict ;

my $major = -1;
my $minor = -1 ;
my $ocaml_version = `ocamlfind ocamlc -version` ;
if ($ocaml_version =~ /^([0-9]+)\.([0-9]+)\./) {
    $major = $1 ;
    $minor = $2 ;
} else {
    exit 255 ;
}

foreach my $line (<STDIN>) {
    if ($line =~ /^##V([^#]+)##/) {
        my $ver = $1 ;
        $line =~ s/^##V[^#]+##// ;
        my $pass = 0 ;
        if ($ver =~ /^[0-9]+$/) {
            $pass = $ver <= $major ;
        } elsif ($ver =~ /^([0-9]+)\.([0-9]+)$/) {
            my $ver_maj = $1 ;
            my $ver_min = $2 ;
            $pass = ($ver_maj <= $major) && ($ver_min <= $minor) ;
        }
        printf "%s", $line if $pass ;
    } else {
        printf "%s", $line ;
    }
}
