#!/usr/bin/perl

foreach (@ARGV) {
  my @command = ('make', 'test', "TEST_FILES=$_", 'TEST_VERBOSE=1', 'MASON_NO_CLEANUP=1');
  print "@command\n";
  system @command;
}
