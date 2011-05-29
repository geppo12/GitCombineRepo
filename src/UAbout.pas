{
	 Copyright 2011 Giuseppe Monteleone

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
unit UAbout;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TfmAbout = class(TForm)
    lblAppName: TLabel;
    btnOK: TButton;
    Label1: TLabel;
    LinkLabel1: TLinkLabel;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure LinkLabel1LinkClick(Sender: TObject; const Link: string; LinkType:
        TSysLinkType);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmAbout: TfmAbout;

implementation

uses
  ShellApi,
  UFileVersion;

{$R *.dfm}

procedure ShellOpen(const Url: string; const Params: string = '');
begin
  ShellExecute(0, 'Open', PChar(Url), PChar(Params), nil, SW_SHOWNORMAL);
end;

procedure TfmAbout.FormCreate(Sender: TObject);
begin
  lblAppName.Caption := lblAppName.Caption + VersionInformation;
end;

procedure TfmAbout.LinkLabel1LinkClick(Sender: TObject; const Link: string;
    LinkType: TSysLinkType);
begin
  ShellOpen(Link);
  Timer1.Enabled := false;
end;

procedure TfmAbout.Timer1Timer(Sender: TObject);
begin
  Modalresult := mrOk;
end;

end.
