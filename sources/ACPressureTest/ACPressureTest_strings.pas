unit ACPressureTest_strings;

{ *******************************************************

  Automated pressure test for AC/ACC

  Computes correct cold pressures to achieve target
  hot pressures

  *******************************************************

  (C) 2021. Ángel Fernández Pineda. Madrid. Spain.

  This work is licensed under the Creative Commons
  Attribution-ShareAlike 3.0 Unported License. To
  view a copy of this license,
  visit http://creativecommons.org/licenses/by-sa/3.0/
  or send a letter to Creative Commons,
  444 Castro Street, Suite 900,
  Mountain View, California, 94041, USA.

  *******************************************************

  [2021-03-17] First implementation

  ******************************************************* }

interface

// Static strings implemented as global variables in order to
// provide further language translation

var
  str_FL: string = 'Front Left';
  str_FR: string = 'Front Right';
  str_PSI: string = ' PSI';
  str_RL: string = 'Rear Left';
  str_RR: string = 'Rear Right';
  str_airTemp: string = 'Air Temp.';
  str_carModel: string = 'Car model';
  str_carPos: string = 'Position';
  str_cold: string = ' (cold)';
  str_compound: string = 'Compound';
  str_dry: string = 'Dry';
  str_error: string = 'ERROR';
  str_hint_finished: string = 'Copy results or restart a new test';
  str_hint_ongoing: string = 'Drive a few laps to measure hot pressures';
  str_hint_unavailable: string = 'Game not running or not in driving session';
  str_hint_waitingColdPressures
    : string = 'Go to track to measure cold pressures';
  str_hint_waitingForGarage: string = 'Return to garage and enter setup screen';
  str_hot: string = ' (hot)';
  str_hotlapNotSuitable: string = 'Hotlap session not suitable for testing';
  str_invalidValue: string = 'Invalid value';
  str_laps: string = 'Laps';
  str_roadTemp: string = 'Road Temp.';
  str_state_Ongoing: string = 'Running pressure test';
  str_state_finished: string = 'Finished';
  str_state_unavailable: string = 'Not ready';
  str_state_waitingColdPressures: string = 'Waiting for cold pressures';
  str_state_waitingForGarage: string = 'Waiting for car at garage';
  str_trackName: string = 'Track name';
  str_weather_options: string = 'Wet/dry';
  str_wet: string = 'Wet';
  str_sponsor: string = 'Sponsored by JJ Endurance Series';
  str_sponsor_link: string ='https://twitter.com/JjEndurance/status/1371506444001030146';

implementation

end.
