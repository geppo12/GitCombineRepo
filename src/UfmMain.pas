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
unit UFmMain;

// ..put some test path inside the program
{.$DEFINE _DEBUGPATH}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, FileCtrl,
  UFileVersion,
  UFmProgress,
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
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    FProgressForm: TfmProgress;
    FGit: TCRGitInterface;
    FAuxStringList: TStringList;
    FCreateErrorMsg: string;
    FLastSelectDir: string;

    procedure siInit;
    procedure copyFileAndDir(AFrom, ATo: string);
    procedure deleteFileAndDir(AName:  string);
    procedure copyOrDeleteDir(AFrom, ATo: string; ADelete: Boolean);
    function extractSubPath(AString: string): string;
    function pathToName(AString: string): string;
    function createProjectDir(AString: string): string;
    procedure processRepo(APath: string);
    procedure mergeRepo(APath: string);
    procedure progressProcess(AStep: TCRGitProgressStep; APosition: Integer; AText: string = '');
  public
    { Public declarations }
  end;

var
  fmMain: TfmMain;

implementation

uses
  Types,
  ShellApi,
  StrUtils,
  SiAuto, UAbout;

const
  kGitDir = '.git';
  kBackupDir = '__bakup_git';


{$R *.dfm}

procedure TfmMain.btnAddClick(Sender: TObject);
begin
  if SelectDirectory('Add Repository','',FLastSelectDir,[sdNewUI,sdNewFolder]) then
    if SysUtils.DirectoryExists(FLastSelectDir + '/'+ kGitDir) then
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
    FGit.OnProgress := progressProcess;
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
  if SysUtils.DirectoryExists(eDestRepo.Text) then begin
    btnGo.Enabled := false;
    FProgressForm := TfmProgress.Create(self);
    FProgressForm.Show;
    lbLog.Clear;
    for LRepo in lbSourceRepo.Items do
      processRepo(LRepo);
    SetCurrentDir(eDestRepo.Text);
    FGit.InitMerge;
    for LRepo in lbSourceRepo.Items do
      mergeRepo(LRepo);
    FGit.FinalizeMerge;
    MoveFile(kBackupdir,kGitDir);
    FreeAndNil(FProgressForm);
    btnGo.Enabled := true;
  end;
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

procedure TfmMain.copyFileAndDir(AFrom, ATo: string);
begin
  if FileGetAttr(AFrom, false) and faDirectory <> 0 then begin
    if not SysUtils.DirectoryExists(ATo) then begin
      SiMain.LogDebug('CreateDir: %s\%s',[GetCurrentDir,ATo]);
      CreateDir(ATo);
    end;
    copyOrDeleteDir(AFrom,ATo,false);
  end else
    if not CopyFile(PChar(AFrom),PChar(ATo),false) then
      SiMain.LogWarning('Fail copy %s->%s',[AFrom,ATo])
    else
      SiMain.LogDebug('Copied file %s->%s',[AFrom,ATo])
end;

procedure TfmMain.deleteFileAndDir(AName:  string);
var
  LAttr: Integer;
begin
  LAttr := FileGetAttr(AName, false);
  if LAttr and faDirectory <> 0 then begin
    copyOrDeleteDir(AName,'',true);
    RemoveDir(AName);
  end else begin
    if (LAttr and faReadOnly) <> 0 then begin
      LAttr := LAttr xor faReadOnly;
      FileSetAttr(AName,LAttr);
    end;
    if not DeleteFile(AName) then
      SiMain.LogWarning('Fail delete %s',[AName]);
  end;
end;

procedure TfmMain.copyOrDeleteDir(AFrom, ATo: string; ADelete: Boolean);
var
  LSearchRec: TSearchRec;
begin
  if FindFirst(AFrom+'\*.*', faAnyFile, LSearchRec) = 0 then
  begin
    repeat
      if (LSearchRec.Name <> '.') and (LSearchRec.Name <> '..') then
        // recursive call to copyFileAndDir
        if ADelete then
          deleteFileAndDir(AFrom+'\'+LSearchRec.Name)
        else
          copyFileAndDir(AFrom+'\'+LSearchRec.Name,ATo+'\'+LSearchRec.Name);
    until FindNext(LSearchRec) <> 0;
    FindClose(LSearchRec);
  end;
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
  LId := 0;
  repeat
    Result := IncludeTrailingBackslash(AString) +  ExtractFileName(AString);
    if LId > 0 then
      Result := Result + IntToStr(LId);
    Inc(LId);
  until not SysUtils.DirectoryExists(Result);
end;

procedure TfmMain.FormClose(Sender: TObject; var Action: TCloseAction);
var
  LForm: TForm;
begin
  LForm := TfmAbout.Create(Self);
  LForm.ShowModal;
end;

procedure TfmMain.FormShow(Sender: TObject);
begin
  if FCreateErrorMsg <> '' then begin
    ShowMessage(FCreateErrorMsg);
    Close;
  end;
{$IFDEF _DEBUGPATH}
  lbSourceRepo.AddItem('D:\DATI\ProgettiRcs\_interni_\GitCombineRepo\test.2\',nil);
  //lbSourceRepo.AddItem('D:\DATI\ProgettiRcs\_interni_\GitCombineRepo\TortoiseGit\',nil);
  eDestRepo.Text := 'c:\tmp---';
{$ENDIF}
end;

procedure TfmMain.processRepo(APath: string);
var
{  cmd: string;
  LFile: string;
  LDir: string; #OC}
  LRootDir: string;
begin
  try
    if SysUtils.DirectoryExists(APath) then begin
      SiMain.LogVerbose('Processing repo: ' + APath);
      SetCurrentDir(APath);
      FProgressForm.Max(0);
      FProgressForm.StepText := 'Backup repository';
      copyFileAndDir(kGitDir,kBackupDir);
      LRootDir := createProjectDir(APath);
      // TRICK: we don't have ending slash, so we consider it as file
      FGit.UpdateCommitTree(ExtractFileName(LRootDir));
    end;
  except
    on EGitError do
      SiMain.LogException;
  end;
end;

procedure TfmMain.mergeRepo(APath: string);
begin
  FProgressForm.Max(0);
  FProgressForm.StepText := 'Merge repository';
  APath := ExcludeTrailingBackslash(APath);
  FGit.MergeRepo(APath);
  FProgressForm.StepText := 'Cleanup';
  deleteFileAndDir(APath+'\'+kGitDir);
  MoveFile(PChar(APath+'\'+kBackupDir),PChar(APath+'\'+kGitDir));
end;

procedure TfmMain.progressProcess(AStep: TCRGitProgressStep; APosition:
    Integer; AText: string = '');
begin
  Application.ProcessMessages;
  if FProgressForm <> nil then
    case AStep of
      kCount: begin
          FProgressForm.StepText := AText;
          FProgressForm.Max(APosition);
        end;
      kPosition: FProgressForm.Progress(APosition);
    end;
end;

end.
