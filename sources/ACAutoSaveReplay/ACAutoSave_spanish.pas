unit ACAutoSave_spanish;

{ *******************************************************

  Auto save replay for AC/ACC

  Spanish translation

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

  [2021-04-06] Added support for any configured
  key to save replay

  [2021-04-08] Added autoload of config files when changed

  [2021-04-11] Translation of new strings

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
  str_replay_cfg_values :=
    'Tiempo m�x. repetici�n: %d segundos, autosalvado: %d';
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
  str_smallAutoReplay :=
    'El tiempo m�x. de repetici�n es demasiado bajo. DESHABILITANDO.';
  str_session_race := 'Sesi�n de carrera';
  str_session_qualy := 'Sesi�n clasificatoria';
  str_session_other := 'No es sesi�n de carrera/clasificatoria';
  str_phase_pre := 'preliminares';
  str_phase_race := 'carrera/calificatoria en curso';
  str_phase_post := 'carrera finalizada';
  str_rejected := 'Registro UDP rechazado';
  str_key_literal := 'Tecla: ';
  str_defaultKey := 'Usando tecla por defecto ("M")';
  str_unsupportedKey_error := 'Tecla no soportada: "%s"';
  str_nextAutosave:= 'Siguiente autosalvado a ';
  str_nextAutosaveOrAtEOS:= ' o al finalizar la sesi�n';
  str_nextAutosaveEOS:= 'Siguiente autosalvado al final de la sesi�n';
  str_nextAutosaveNotNeeded:='Siguiente autosalvado a cargo de ACC';
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
      Menu_reload.Caption := 'Recargar ficheros de configuraci�n';
      Caption := 'Autosalvar repetici�n';
    end;
end;

initialization

I18NUtils.RegisterClass(TSpanishTranslator);

end.
