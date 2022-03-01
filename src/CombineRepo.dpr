program CombineRepo;

uses
  EMemLeaks,
  Forms,
  UFmMain in 'UFmMain.pas' {fmMain},
  UFileVersion in 'UFileVersion.pas',
  UGit in 'UGit.pas',
  UPipeStream in 'UPipeStream.pas',
  UFmProgress in 'UFmProgress.pas' {fmProgress},
  UAbout in 'UAbout.pas' {fmAbout};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'CombineRepo';
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.

