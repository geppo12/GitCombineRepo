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
unit UFmMain;

// ..put some test path inside the program
{$DEFINE _DEBUGPATH}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, FileCtrl,
  UFileVersion,
  UGit;

type
  TfmMain = class(TForm)
    eDestRepo: TEdit;
    btnGo: TButton;
    lbSourceRepo: TListBox;
    Label1: TLabel;
    btnAdd: TButton;
    cbBackup: TCheckBox;
    btnSelect: TButton;
    lbLog: TListBox;
    Label2: TLabel;
    procedure btnAddClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnGoClick(Sender: TObject);
    procedure btnSelectClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FGit: TCRGitInterface;
    FAuxStringList: TStringList;
    FCreateErrorMsg: string;
    FLastSelectDir: string;

    procedure siInit;
    function extractSubPath(AString: string): string;
    function pathToName(AString: string): string;
    function createProjectDir(AString: string): string;
    function createSubDir(ARoot,APath: string): string;
    procedure processRepo(APath: string);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  fmMain: TfmMain;

implementation

uses
  Types,
  StrUtils,
  SiAuto;

{$R *.dfm}

procedure TfmMain.btnAddClick(Sender: TObject);
begin
  if SelectDirectory('Add Repository','',FLastSelectDir,[sdNewUI,sdNewFolder]) then
    if SysUtils.DirectoryExists(FLastSelectDir + '/.git') then
      lbSourcerepo.Items.Add(FLastSelectDir)
    else
      ShowMessage('Add a git repository');
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  FGit.Free;
  FAuxStringList.Free;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  siInit;
  try
    FGit := TCRGitInterface.Create;
    FAuxStringList := TStringList.Create;
    Caption := Application.Title;
    Caption := Caption + ' ' + VersionInformation;
  except
    on E: Exception do begin
      FCreateErrorMsg := E.Message;
    end;
  end;
end;

procedure TfmMain.btnGoClick(Sender: TObject);
var
  LRepo: string;
begin
  btnGo.Enabled := false;
  if SysUtils.DirectoryExists(eDestRepo.Text) then begin
    lbLog.Clear;
    for LRepo in lbSourceRepo.Items do
      processRepo(LRepo);
    { TODO 1 -cFIXME : restore merge of precessed repos
    SetCurrentDir(eDestRepo.Text);
    FGit.InitRepo;
    for LRepo in lbSourceRepo.Items do
      FGit.PullRepo(LRepo); }
  end;
  btnGo.Enabled := true;
end;

procedure TfmMain.btnSelectClick(Sender: TObject);
begin
  if SelectDirectory('Destination','',FLastSelectDir,[sdNewUI,sdNewFolder]) then
    eDestRepo.Text := FLastSelectDir;
end;

procedure TfmMain.siInit;
var
  LFileNameSic: string;
  LFileNameSil: string;
  LUserDir: string;
  sicDone: Boolean;
begin
  { suppongo che il file *.sic esita }
  sicDone := true;
  LUserDir := ExtractFilePath(Application.ExeName);

  { creo  il nome opportuno per il file *.sic corrente nella directory del applicativo }
  LFileNameSic := ChangeFileExt(Application.ExeName,'.sic');
  if not FileExists(LFileNameSic) then begin
    { se non esiste nella directory del programma provo nella directory utente }
    LFileNameSic := LUserDir + ExtractFileName(LFileNameSic);
    { se non esiste nemmeno qui marco non esistente }
    if not FileExists(LFileNameSic) then
      sicDone := false;
  end;

  { se l'ho trovato simposto le variabili e carico la configurazione }
  if sicDone then begin
    { filename di default (stesso path del file sic) }
    Si.SetVariable('DefFilename',ChangeFileExt(LFileNameSic,'.sil'));
    { file name con path utente }
    LFileNameSil := ChangeFileExt(LUserDir + ExtractFileName(LFileNameSic),'.sil');
    Si.SetVariable('UserFilename',LFileNameSil);
    { imposto la password per la crittografia del log }
    Si.SetVariable('Password','Luciano');
    { carico la configurazione dal file sic }
    Si.LoadConfiguration(LFileNameSic);
  end;

{$IFDEF _MICROSEC}
  Si.Resolution := crHigh;
{$ENDIF}
  SiMain.ClearAll;

  SiMain.LogVerbose('Read SmartInspect config %s',[LFileNameSic]);

{$IFNDEF EUREKALOG}
  Application.OnException := SiMain.ExceptionHandler;
{$ENDIF}
end;

function TfmMain.extractSubPath(AString: string): string;
var
  LArray: TStringDynArray;
  I: Integer;
begin
  LArray := SplitString(AString,'\/');
  Result := '';
  for I := 0 to Length(LArray) - 2 do
    Result := Result + LArray[I] + '/';
  Result := LeftStr(Result,Length(Result)-1);
end;

function TfmMain.pathToName(AString: string): string;
var
  LArray: TStringDynArray;
  LPos,
  I: Integer;
begin
  LArray := SplitString(AString,'\/:');
  Result := '';
  for I := 0 to Length(LArray) - 2 do
    Result := Result + '_' + LArray[I];
end;

// create a suitable prject dir
function TfmMain.createProjectDir(Astring: string): string;
var
  LArray: TStringDynArray;
  LDir: string;
  LId: Integer;
begin
  // TRICK: strip ending backslash so we can consider it aa file name
  AString := ExcludeTrailingBackslash(AString);
  repeat
    Result := IncludeTrailingBackslash(AString) +  ExtractFileName(AString);
    if LId > 0 then
      Result := Result + IntToStr(LId);
    Inc(LId);
  until not SysUtils.DirectoryExists(Result);
  // create dir with DOS slash
  CreateDir(Result);
end;

function TfmMain.createSubDir(ARoot,APath: string): string;
var
  LArray: TStringDynArray;
  LDir,
  LStr: string;
begin
  LDir := ARoot;
  if APath <> '' then begin
    LArray := SplitString(APath,'/');
    for LStr in LArray do begin
      LDir := LDir + '/' + LStr;
      CreateDir(LDir);
      SetCurrentDir(LDir);
    end;
    SetCurrentDir(ARoot);
  end;
  Result := LDir;
end;

procedure TfmMain.FormShow(Sender: TObject);
begin
  if FCreateErrorMsg <> '' then begin
    ShowMessage(FCreateErrorMsg);
    Close;
  end;
{$IFDEF _DEBUGPATH}
  lbSourceRepo.AddItem('D:\DATI\ProgettiRcs\_interni_\GitCombineRepo\test\',nil);
  eDestRepo.Text := 'c:\tmp---';
{$ENDIF}
end;

procedure TfmMain.processRepo(APath: string);
var
  cmd: string;
  LFile: string;
  LDir: string;
  LRootDir: string;
begin
  try
    if SysUtils.DirectoryExists(APath) then begin
      SiMain.LogVerbose('Processing repo: ' + APath);
      SetCurrentDir(APath);
      // #DEBUG
      SiMain.LogDebug('Dir: %s',[GetCurrentDir]);
      // create project subdirectory
      LRootDir := createProjectDir(APath);
      // create unix slash
      //LRootDir := ReplaceStr(LRootDir,'\','/');
      // TRICK: we don't have ending slash, so we consider it as file
      FGit.UpdateCommitTree(ExtractFileName(LRootDir));
    end;
  except
    on EGitError do
      SiMain.LogException;
  end;
end;

end.
