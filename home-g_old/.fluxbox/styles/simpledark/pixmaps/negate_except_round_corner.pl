#!/usr/bin/perl

use strict; use warnings;
use GD;

my $file_orig = $ARGV[0];
my $file_out  = $ARGV[1];
my $corner    = $ARGV[2];

die "Number of args must be 3" unless $#ARGV+1 == 3;
die "3rd argument must be topLeft or topRight: $corner"
    unless $corner eq "topLeft" or $corner eq "topRight";

my $file_temp = "temp.png";

system "convert $file_orig -negate $file_temp";

open (PNG,$file_temp) || die;
my $img = newFromPng GD::Image(\*PNG) || die;
close PNG;

my $color = $img->colorAllocate(48, 48, 80);
if ($corner eq "topLeft") {
    $img->setPixel(0,  0,  $color);
    $img->setPixel(1,  0,  $color);
    $img->setPixel(2,  0,  $color);
    $img->setPixel(3,  0,  $color);
    $img->setPixel(4,  0,  $color);
    $img->setPixel(0,  1,  $color);
    $img->setPixel(1,  1,  $color);
    $img->setPixel(2,  1,  $color);
    $img->setPixel(0,  2,  $color);
    $img->setPixel(1,  2,  $color);
    $img->setPixel(0,  3,  $color);
    $img->setPixel(0,  4,  $color);
} elsif ($corner eq "topRight") {
    $img->setPixel(11, 0,  $color);
    $img->setPixel(12, 0,  $color);
    $img->setPixel(13, 0,  $color);
    $img->setPixel(14, 0,  $color);
    $img->setPixel(15, 0,  $color);
    $img->setPixel(13, 1,  $color);
    $img->setPixel(14, 1,  $color);
    $img->setPixel(15, 1,  $color);
    $img->setPixel(14, 2,  $color);
    $img->setPixel(15, 2,  $color);
    $img->setPixel(15, 3,  $color);
    $img->setPixel(15, 4,  $color);
}

$img->_file($file_temp);

system "convert $file_temp $file_out";
system "rm $file_temp";
