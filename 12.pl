#!/usr/local/bin/perl

while(<>) {
    s/<x=(-?\d+), y=(-?\d+), z=(-?\d+).+$/$1 $2 $3/;
    print;
}
