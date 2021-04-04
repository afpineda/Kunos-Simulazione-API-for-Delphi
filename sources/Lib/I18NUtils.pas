// ----------------------------------------------------------------------------
unit I18NUtils;
// ----------------------------------------------------------------------------

{
  *****************************************************************************
  Internationalization utilities
  *****************************************************************************
  (C) 2010. Angel Fernandez Pineda. Madrid. Spain.

  This work is licensed under the Creative Commons
  Attribution-ShareAlike 3.0 Unported License. To
  view a copy of this license,
  visit http://creativecommons.org/licenses/by-sa/3.0/
  or send a letter to Creative Commons,
  444 Castro Street, Suite 900,
  Mountain View, California, 94041, USA.
}
// ----------------------------------------------------------------------------
{
  USAGE:

  - Develop your application in the default language. Note that users with
  non-translated locales will see the default language.
  - Declare all strings as global variables in a separate unit. For example,
  "strings.pas".
  - Create a unit for every language. Those are the translation units.
  For example, "korean.pas" and "spanish.pas"
  - Add the "Windows" and "I18NUtils" units to the uses clause of translation
  units. Also import all units that require translation.
  - Derive a translation class from TI18NTranslator for each language.
  For example:

  type TSpanishTranslator = class(TI18NTranslator) ...

  - Add an "initialization" section to every translation unit and call
  "I18NUtils.RegisterClass(<<your_translation_class>>)".
  For example:

  initialization
  RegisterClass(TSpanishTranslator);
  end.

  - Redefine "GetLanguageID" to return the languageID constant for your
  translation class. For Example:

  class function TSpanishTranslator.GetLanguageID : LANGID;
  begin
  Result := LANG_SPANISH
  end;

  - Write a "SetTranslation" method to translate all global strings and
  perform any other initialization
  - Write a "TranslateObject" method. Test if an object is derived from
  a class which needs translation and do so.
  For example:

  if (Instance is TForm1) then Translate_Form1(Instance as TForm1);

  - Hook the OnCreate Event of every form that needs translation and call
  "I18NUtils.Translate(self)"
  - Redefine "IsDefault" method to return "true" if you change your mind
  about the default language.
}
// ----------------------------------------------------------------------------

interface

uses
  Windows;

// ----------------------------------------------------------------------------
// Classes
// ----------------------------------------------------------------------------

type

  TI18NTranslator = class(TObject)
  public
    class function IsDefault: boolean; virtual;
    class function GetLanguageID: LANGID; virtual; abstract;
    class procedure SetTranslation; virtual; abstract;
    class procedure TranslateObject(Instance: TObject); virtual;
  end;

  TI18N = class of TI18NTranslator;

  // --------------------------------------------------------------------------
  // Exports
  // --------------------------------------------------------------------------

function GetCurrentLanguageID: WORD;
procedure RegisterClass(aClass: TI18N);

procedure Translate(aObject: TObject);

// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------
implementation

// ----------------------------------------------------------------------------
// ----------------------------------------------------------------------------

uses
  Forms,
  SysUtils;

var
  Translator: TI18N = nil;

  // --------------------------------------------------------------------------
  // MACRO: Get Current language identifier
  // --------------------------------------------------------------------------

function GetCurrentLanguageID: WORD;
begin
  Result := Windows.GetUserDefaultLangID and $00FF;
end;

// ----------------------------------------------------------------------------
// Translate object
// ----------------------------------------------------------------------------

procedure Translate(aObject: TObject);
begin
  if (Translator <> nil) then
    Translator.TranslateObject(aObject);
end;

// ----------------------------------------------------------------------------
// TI18NTranslator class
// ----------------------------------------------------------------------------

class procedure TI18NTranslator.TranslateObject(Instance: TObject);
begin
  // do nothing
end;

// ----------------------------------------------------------------------------

class function TI18NTranslator.IsDefault: boolean;
begin
  Result := false;
end;

// ----------------------------------------------------------------------------
// Set active translator
// ----------------------------------------------------------------------------

procedure RegisterClass(aClass: TI18N);
begin
  if ((Translator = nil) and (aClass.IsDefault)) or
    (GetCurrentLanguageID = aClass.GetLanguageID) then
  begin
    Translator := aClass;
    if (Translator <> nil) then
      Translator.SetTranslation;
  end;
end;

// ----------------------------------------------------------------------------

end.
