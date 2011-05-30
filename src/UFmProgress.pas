unit UFmProgress;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls;

type
  TfmProgress = class(TForm)
    pbProgress: TProgressBar;
    lblCount: TLabel;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    FMax: Integer;
    FStepText: string;

    procedure setStepText(AText: string);

  public
    { Public declarations }
    procedure Max(AMax: Integer);
    procedure Progress(APosition: Integer);
    property StepText: string read FStepText write setStepText;
  end;

var
  fmProgress: TfmProgress;

implementation

{$R *.dfm}

procedure TfmProgress.setStepText(AText: string);
begin
  FStepText := AText;
  if FMax = 0 then begin
    lblCount.Caption := FStepText;
    Application.ProcessMessages;
  end;
end;

procedure TfmProgress.FormShow(Sender: TObject);
begin
  pbProgress.Position := 0;
  pbProgress.Min := 0;
end;

procedure TfmProgress.Max(AMax: Integer);
begin
  if AMax > 0 then begin
    pbProgress.Max := AMax;
    pbProgress.Visible := true;
  end else
    pbProgress.Visible := false;

  Application.ProcessMessages;
  FMax := AMax;
end;

procedure TfmProgress.Progress(APosition: Integer);
begin
  pbProgress.Position := APosition;
  if FStepText <> '' then
    lblCount.Caption := Format('%s %d / %d',[FStepText,APosition,FMax])
  else
    lblCount.Caption := Format('Processing %d / %d',[APosition,FMax]);
end;

end.
