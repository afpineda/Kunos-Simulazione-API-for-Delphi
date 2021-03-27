program ACPressureTest;

uses
  Vcl.Forms,
  ACPressureTest_main in '..\sources\ACPressureTest\ACPressureTest_main.pas' {Form_main},
  I18NUtils in '..\sources\Lib\I18NUtils.pas',
  ACPressureTest_strings in '..\sources\ACPressureTest\ACPressureTest_strings.pas',
  ACPressureTest_state in '..\sources\ACPressureTest\ACPressureTest_state.pas',
  ksSharedMem in '..\sources\ksSharedMem.pas',
  ksSharedMem.Utils in '..\sources\ksSharedMem.Utils.pas',
  ksSharedMem.Data in '..\sources\ksSharedMem.Data.pas',
  ACPressureTest_CopyPasteOptions in '..\sources\ACPressureTest\ACPressureTest_CopyPasteOptions.pas' {Form_CopyPasteOptions},
  ACPressureTest_spanish in '..\sources\ACPressureTest\ACPressureTest_spanish.pas',
  ACPressureTest_about in '..\sources\ACPressureTest\ACPressureTest_about.pas' {Form_About};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'AC/ACC Automated Pressure Test';
  Application.CreateForm(TForm_main, Form_main);
  Application.CreateForm(TForm_CopyPasteOptions, Form_CopyPasteOptions);
  Application.CreateForm(TForm_About, Form_About);
  Application.Run;
end.
