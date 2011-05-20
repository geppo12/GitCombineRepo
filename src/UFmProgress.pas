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

  public
    { Public declarations }
    procedure Max(AMax: Integer);
    procedure Progress(APosition: Integer);
  end;

var
  fmProgress: TfmProgress;

implementation

{$R *.dfm}

procedure TfmProgress.FormShow(Sender: TObject);
begin
  pbProgress.Position := 0;
  pbProgress.Min := 0;
end;

procedure TfmProgress.Max(AMax: Integer);
begin
  pbProgress.Max := AMax;
  FMax := AMax;
end;

procedure TfmProgress.Progress(APosition: Integer);
begin
  pbProgress.Position := APosition;
  lblCount.Caption := Format('Processing %d / %d',[APosition,FMax]);
end;

end.
