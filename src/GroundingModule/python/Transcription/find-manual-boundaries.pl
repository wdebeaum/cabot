#!/usr/bin/perl

# find-manual-boundaries.pl - find utterance boundaries manually

use strict vars;

my $usage = <<EOU;
USAGE:
$0 abs-ref-time audio.wav input-transcript.txt output-transcript.txt

abs-ref-time
	the absolute time of the reference point, in seconds since the epoch
audio.wav
	the audio that was transcribed
input-transcript.txt
	a text file to read utterances from (split on periods, question/exclamation marks, and newlines)
output-transcript.txt
	a transcript file to write, suitable for input to TranscriptReader

The audio will begin playing immediately. Press enter at the reference point.
Then the first utterance will be displayed. Press enter when the audio reaches
the cursor (at the start or the end of the last displayed utterance).


EOU

use Time::HiRes qw(time);

die $usage unless (@ARGV == 4);

my ($abs_ref_time, $audio_file, $input_file, $output_file) = @ARGV;
@ARGV=();

open INPUT, "<$input_file"
  or die "Can't open input transcript '$input_file': $!\n";
my @input_utterances =
  map { s/^\s*//; s/\s*$//; $_ }
  map { split(/[\.\?!]/) }
  <INPUT>;
close INPUT;


my $pid = fork;
unless ($pid) {


  # AUDIO PLAYERS
  # uncomment the one you want to use

  # Mac OS X command line audio player
  #`afplay $audio_file 2>&1 >/dev/null`;
  
  # ALSA audio player (Linux only)
  #`aplay $audio_file 2>&1 >/dev/null`;

  # VideoLAN
  #`vlc -I dummy $audio_file 2>&1 >/dev/null`;
  `/Applications/VLC.app/Contents/MacOS/VLC -I dummy $audio_file 2>&1 >/dev/null`;

  # MPlayer
  #`mplayer --really-quiet $audio_file 2>&1 >/dev/null`;
  
  # Part of ffmpeg package; unfortunately pops up window even for audio only
  #`ffplay $audio_file 2>&1 >/dev/null`;


} else {
  print "player PID = $pid\n";
  open OUTPUT, ">$output_file"
    or die "Can't open output transcript '$output_file': $!\n";
  print "press enter at the reference point\n";
  my $foo = <>;
  my $rel_ref_time = time;

  for (@input_utterances) {
    print ' ' . substr($_, 0, 72) . "\r";
    my $enter = <>;
    printf OUTPUT '%.2f', (time - $rel_ref_time + $abs_ref_time);
    print "\033[F"; # go up a line
    print " $_";
    $enter = <>;
    printf OUTPUT " %.2f %s\n", (time - $rel_ref_time + $abs_ref_time), $_;
  }
  close OUTPUT;
  print "Waiting for player to finish...\n";
  waitpid $pid, 0;
}

