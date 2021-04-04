unit ACAutoSave_spanish;

{ *******************************************************

  Auto save replay for AC/ACC

  Sends the "save replay" key to ACC at regular intervals

  *******************************************************

  (C) 2021. �ngel Fern�ndez Pineda. Madrid. Spain.

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

uses
  Windows,
  I18NUtils;

type
  TSpanishTranslator = class(TI18NTranslator)
  public
    class function GetLanguageID: LANGID; override;
    class procedure SetTranslation; override;
    class procedure TranslateObject(Instance: TObject); override;
  end;

implementation

uses
  ACAutoSave_main,
  ACAutoSave_strings;

class function TSpanishTranslator.GetLanguageID: LANGID;
begin
  Result := LANG_SPANISH;
end;

class procedure TSpanishTranslator.SetTranslation;
begin
  str_jsonFieldNotFound := 'campo requerido no encontrado';
  str_readingGameCfg := 'Leyendo ficheros de configuraci�n del juego';
  str_error := 'Error';
  str_ok := 'Ok';
  str_broadcast_cfg_values := 'Puerto UDP: %d';
  str_replay_cfg_values := 'Tiempo m�x. repetici�n: %d segundos, autosalvado: %d';
  str_start_protocol := 'Iniciando protocolo de retransmisi�n';
  str_state_notRegistered := 'Inactivo';
  str_state_waiting := 'Esperando comienzo de carrera/clasificaci�n';
  str_state_inProgress := 'Carrera en progreso';
  str_autosaving := 'Autosalvando';
  str_copyright_notice1 :=
    'Este software est� en el dominio p�blico. PROHIBIDA SU VENTA';
  str_copyright_notice2 := 'Patrocinado por JJ Endurance Series';
  str_disable_warning := 'DESHABILITADO. Sin autosalvado';
  str_enable_warning := 'HABILITADO';
  str_smallAutoReplay := 'El tiempo m�x. de repetici�n es demasiado bajo. Deshabilitando.';
  str_session_race := 'Sesi�n de carrera';
  str_session_qualy := 'Sesi�n clasificatoria';
  str_session_other := 'No es sesi�n de carrera/clasificatoria';
  str_phase_pre := 'preliminares';
  str_phase_race := 'carrera en curso';
  str_phase_post := 'carrera finalizada';
end;

class procedure TSpanishTranslator.TranslateObject(Instance: TObject);
begin
  if Instance is TForm_main then
    With (Instance as TForm_main) do
    begin
      Menu_Action.Caption := '&Acci�n';
      Menu_ClearLog.Caption := '&Limpiar bit�cora';
      Menu_disable.Caption := 'Deshabilitar';
      Menu_Checks.Caption := 'Comprobaciones';
      Menu_saveNow.Caption := 'Salvar repetici�n ahora (para probar)';
      Menu_ShowState.Caption := 'Mostrar estado de la sesi�n';
      Menu_About.Caption := 'Acerca de';
      Caption := 'Autosalvar repetici�n';
    end;
end;

initialization

I18NUtils.RegisterClass(TSpanishTranslator);

end.
