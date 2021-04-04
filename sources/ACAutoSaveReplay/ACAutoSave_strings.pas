unit ACAutoSave_strings;
{ *******************************************************

  Auto save replay for AC/ACC

  Sends the "save replay" key to ACC at regular intervals

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

  [2021-04-04] First implementation

  ******************************************************* }

interface

var
  str_line: string = '--------------------------------------------------';
  // str_notime: string = '--:--:--';
  str_copyright_notice
    : string = '(C) 2021. Ángel Fernández Pineda. Madrid. Spain';

var
  str_jsonFieldNotFound: string = 'required field not found';
  str_readingGameCfg: string = 'Reading game configuration files';
  str_error: string = 'Error';
  str_ok: string = 'Ok';
  str_broadcast_cfg_values: string = 'UDP Port: %d';
  str_replay_cfg_values: string = 'Max time replay: %d seconds, autosave: %d';
  str_start_protocol: string = 'Starting broadcasting protocol';
  str_state_notRegistered: string = 'Inactive';
  str_state_waiting: string = 'Waiting for race/qualify to start';
  str_state_inProgress: string = 'Race in progress';
  str_autosaving: string = 'Autosaving';
  str_copyright_notice1
    : string = 'This software is in the public domain. NOT FOR SALE';
  str_copyright_notice2: string = 'Sponsored by JJ Endurance Series';
  str_disable_warning: string = 'DISABLED. No replay saving';
  str_enable_warning: string = 'ENABLED';
  str_smallAutoReplay
    : string = 'Max replay time is too small. Disabling auto save.';
  str_session_race: string = 'Race session';
  str_session_qualy: string = 'Qualifying session';
  str_session_other: string = 'Not race/qualifying session';
  str_phase_pre: string = 'preliminars';
  str_phase_race: string = 'race in progress';
  str_phase_post: string = 'race over';
  str_rejected: string = 'UDP registration rejected';

implementation

end.
