unit ACPressureTest_spanish;

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

uses
  ACPressureTest_CopyPasteOptions,
  ACPRessureTest_main,
  ACPRessureTest_about,
  WinApi.Windows,
  I18NUtils;

type
  TSpanishTranslator = class(TI18NTranslator)
    {
      PORPOUSE: Spanish translation
    }
    class function GetLanguageID: LANGID; override;
    class procedure SetTranslation; override;
    class procedure TranslateObject(Instance: TObject); override;
    class procedure TranslateForm_main(inst: TForm_main);
    class procedure TranslateForm_about(inst: TForm_about);
    class procedure TranslateForm_CopyPasteOptions
      (inst: TForm_CopyPasteOptions);
  end;

implementation

uses
  ACPressureTest_strings;

class procedure TSpanishTranslator.SetTranslation;
begin
  str_FL := 'Del. Izq.';
  str_FR := 'Del. Der.';
  // str_PSI := ' PSI';
  str_RL := 'Tras. Izq.';
  str_RR := 'Tras. Der.';
  str_airTemp := 'Temp. aire';
  str_carModel := 'Modelo';
  str_carPos := 'Posición';
  str_cold := ' (en frio)';
  str_compound := 'Compuesto';
  str_dry := 'Seco';
  // str_error := 'ERROR';
  str_hint_finished := 'Copia los resultados o empieza otro test';
  str_hint_ongoing :=
    'Conduce algunas vueltas para medir la presión en caliente';
  str_hint_unavailable := 'Juego inactivo o no se está conduciendo';
  str_hint_waitingColdPressures :=
    'Vuelve a pista para obtener presiones en frio';
  str_hint_waitingForGarage :=
    'Vuelve al garage y entra en el menu de reglajes';
  str_hot := ' (en caliente)';
  str_hotlapNotSuitable :=
    'Las sesiones de vuelta rápida no sirven para este test';
  str_invalidValue := 'Valor inválido';
  str_laps := 'Vueltas';
  str_roadTemp := 'Temp. pista';
  str_state_Ongoing := 'Ejecutando test de presiones';
  str_state_finished := 'Finalizado';
  str_state_unavailable := 'No preparado';
  str_state_waitingColdPressures := 'Esperando a tomar presiones en frio';
  str_state_waitingForGarage := 'Esperando vuelta al garage';
  str_trackName := 'Circuito';
  str_weather_options := 'Lluvia/seco';
  str_wet := 'Lluvia';
  str_sponsor := 'Patrocinado por JJ Endurance Series';
end;

class function TSpanishTranslator.GetLanguageID: LANGID;
begin
  Result := LANG_SPANISH;
end;

class procedure TSpanishTranslator.TranslateObject(Instance: TObject);
begin
  if (Instance is TForm_main) then
    TranslateForm_main(Instance as TForm_main)
  else if (Instance is TForm_CopyPasteOptions) then
    TranslateForm_CopyPasteOptions(Instance as TForm_CopyPasteOptions)
  else if (Instance is TForm_About) then
    TranslateForm_About(Instance as TForm_About);
end;

class procedure TSpanishTranslator.TranslateForm_main(inst: TForm_main);
begin
  inst.Caption := 'AC/ACC: Test automatizado de presiones';
  inst.Menu_About.Caption := 'Acerca de';
  inst.Menu_action.Caption := '&Acción';
  inst.Menu_globalOptions.Caption := '&Opciones';
  inst.Menu_Restart.Caption := '&Reiniciar test';
  inst.Menu_DefaultTargetPSI.Caption := 'Usar presiones obje&tivo por defecto';
  inst.Menu_copy.Caption := '&Copiar resultados al portapapeles';
  inst.Menu_TestOptions.Caption := 'Opciones del test de &presiones';
  inst.Menu_AppOptions.Caption := 'Opcione&s de esta app';
  inst.Menu_NormalizeAtFinishLine.Caption :=
    'Leer presiones en caliente en línea de &meta';
  inst.Menu_NormalizeAtCarPos.Caption :=
    'Leer presiones en &caliente en la posición actual';
  inst.Menu_CompleteAtTeleport.Caption :=
    'Completar test al &teletransportarse al garage';
  inst.Menu_Min2Laps.Caption :=
    'Completar test tras &2 vueltas (reglajes de calificación)';
  inst.Menu_Min5Laps.Caption := 'Completar test tras &5 vueltas';
  inst.Menu_Min7Laps.Caption :=
    'Completar test tras &7 vueltas (reglajes de carrera)';
  inst.Menu_stayOnTop.Caption := 'Mantener siempre &visible';
  inst.Menu_CopyPasteOptions.Caption := 'Opciones de cop&iar-pegar';
  inst.Menu_cleanReg.Caption :=
    'Limpiar mis claves de registro de Windows (al salir)';
  inst.Lbl_target.Caption := 'Presiones objetivo';
  inst.Lbl_NormalizedPressures.Caption := 'Presiones corregidas (en frio)';
  inst.Chk_Link.Caption := 'Enlazar cajas de edición';
end;

class procedure TSpanishTranslator.TranslateForm_CopyPasteOptions
  (inst: TForm_CopyPasteOptions);
begin
  inst.Caption := 'Opciones de copiar-pegar';
  inst.Btn_up.Caption := 'Subir';
  inst.Btn_down.Caption := 'Bajar';
  inst.Lbl_fields.Caption := 'Campos disponibles';
end;

class procedure TSpanishTranslator.TranslateForm_about(inst: TForm_about);
begin
  inst.Caption := 'Acerca de';
  inst.Lbl_notForSale.Caption :=
    'Este software está en el dominio público. PROHIBIDA SU VENTA.';
  inst.Link_JJ.Caption := str_sponsor;
end;

initialization

I18NUtils.RegisterClass(TSpanishTranslator);

end.
