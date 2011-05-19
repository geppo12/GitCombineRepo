unit UFmProgress;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls;

type
  TfmProgress = class(TForm)
    pbRepo: TProgressBar;
    pbFiles: TProgressBar;
    lblRepo: TLabel;
    lblFiles: TLabel;
  private
    { Private declarations }
    procedure setNumRepo(ANum: Integer);
    function getNumRepo: Integer;
    procedure setNumFiles(ANum: Integer);
    function getNumFiles: Integer;
  public
    { Public declarations }
    property NumRepo: Integer read getNumRepo write setNumRepo;
    property NumFiles: Integer read getNumFiles write setNumFiles;
  end;

var
  fmProgress: TfmProgress;

implementation

{$R *.dfm}

procedure TfmProgress.setNumRepo(ANum: Integer);
begin
  pbRepo.Max := ANum;
end;

function TfmProgress.getNumRepo: Integer;
begin
  Result := pbRepo.Max;
end;

procedure TfmProgress.setNumFiles(ANum: Integer);
begin
  pbFiles.Max := ANum;
end;

function TfmProgress.getNumFiles: Integer;
begin
  Result := pbFiles.Max;
end;

end.
