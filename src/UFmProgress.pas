{
	 Copyright 2011-2022 Giuseppe Monteleone

	 This file is part of 'CombineRepo'

	 'CombineRepo' is free software: you can redistribute
	 it and/or modify it under the terms of the GNU General Public
	 License versione 2, as published by the Free Software Foundation

 	 THIS SOFTWARE IS PROVIDED BY GIUSEPPE MONTELEONE ``AS IS'' AND ANY
 	 EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
	 IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
	 PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL GIUSEPPE MONTELEONE BE
	 LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY,
	 OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
	 OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
	 OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
	 LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
	 NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
	 SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

	 You should have received a copy of the GNU General Public License
	 along with 'CombineRepo'. If not, see <http://www.gnu.org/licenses/>.
}

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
