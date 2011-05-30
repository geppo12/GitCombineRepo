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
unit UGit;

interface

uses
  Generics.Collections,
  SysUtils,
  Classes,
  UPipeStream;

type
  TCRGitCommitInfo = record
    public
    Name: string;
    EMail: string;
    Time: string;
    procedure Parse(AString: string);
  end;

  TCRGitCommit = class
    private
    class var
    FAuxList: TStringList;

    var
    FTree: string;
    FCommit: string;
    // only few string
    FParents: array of string;
    FParentsCount: Integer;
    FAuthor: TCRGitCommitInfo;
    FCommitter: TCRGitCommitInfo;
    FMessage: TStringList;

    procedure addParent(AParent: string);
    procedure parseLine(ALine: string);

    // set/get
    function getParents(AIndex: Integer): string;

    public
    class constructor Create;
    class destructor Destroy;
    constructor Create(ACommit: string; ACommitObj: TCRGitCommit = nil; ATree:
        string = '');
    destructor Destroy; override;
    procedure Parse(AFile: string);
    procedure MessageToStream(AStream: TStringStream);

    property Commit: string read FCommit;
    property Tree: string read FTree;
    property Parents[AIndex: Integer]: string read getParents;
    property ParentsCount: Integer read FParentsCount;
    property Committer: TCRGitCommitInfo read FCommitter;
    property Author: TCRGitCommitInfo read FAuthor;
  end;

  TCRGitProgressStep = (
    kCount,
    kPosition
  );

  TCRGitProgress = procedure(AStep: TCRGitProgressStep; APosition: Integer; AText: string = '') of object;

  TCRGitInterface = class
    private
    FOnProgress: TCRGitProgress;
    FIndexFilename: string;
    FCmdPath: string;
    FAuxList: TStringList;
    FCommitList: TObjectList<TCRGitCommit>;
    //* list references
    FRefList: TStringList;
    FCommitMapper: TStringList;
    FCurrentCommit: string;

    procedure progressIndication(AStep: TCRGitProgressStep; APosition: Integer; AText:string = ''); inline;
    function executeCommand(ACmd: string; AOut: TPipeStream; AIn: TPipeStream = nil): Boolean; overload; inline;
    function executeCommand(ACmd: string; AFile: string = ''; AOut: TPipeStream =
        nil; AIn: TPipeStream = nil): Boolean; overload;
    procedure updateUserEnvVars(AId: string; var AReference: TCRGitCommitInfo;
        ANew: TCRGitCommitInfo);
    procedure findGitPathFormInno;
    procedure setIndexFilename(AName: string);
    function mapCommit(ACommitStr: string): string;
    function createParentStr(ACommit: TCRGitCommit): string;
    procedure updateRefList;
    function refToHead(ARef: string): string;
    procedure updateIndex(APath: string);
    procedure createCommitList;

    public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure List(AList: TStringList);
    procedure Commit(AMessage: string);
    procedure MergeRepo(ARepo: string);
    procedure InitMerge;
    procedure FinalizeMerge;
    procedure UpdateCommitTree(APath: string);
    property OnProgress: TCRGitProgress read FOnProgress write FOnProgress;
  end;

  EGitError = class(Exception);

implementation

uses
  SiAuto,
  SmartInspect,
  Types,
  StrUtils,
  Registry,
  Windows;

const

  kTempName = '.git/git.tmp';
  kErrorName  = '.git/gitError.txt';
  kIndexName = '.git/index';
  kNewIndexName = '.git/index.new';
  // kMsgDir = '.git/rewrite-message';
  kFileConfig = '.git/config';

  // we read install location of Git form uninstall keys of InnoSetup
  kInnoKey =  'Software\Microsoft\Windows\CurrentVersion\Uninstall\Git_is1';
  kInnoValue = 'InstallLocation';


{$REGION 'TCRGitCommitInfo'}
procedure TCRGitCommitInfo.Parse(AString: string);
var
  LPos: Integer;
begin
  LPos := Pos('<',AString);
  Name := Trim(LeftStr(AString,LPos-1));
  AString := RightStr(AString,Length(AString)-LPos);
  LPos := Pos('>',AString);
  EMail := Trim(LeftStr(AString,LPos-1));
  Time := Trim(RightStr(AString,Length(AString)-LPos));
end;

{$ENDREGION}
procedure TCRGitCommit.addParent(AParent: String);
begin
  Inc(FParentsCount);
  SetLength(FParents,FParentsCount);
  FParents[FParentsCount-1] := AParent;
end;

procedure TCRGitCommit.parseLine(ALine: string);
var
  LPos: Integer;
  LKey: string;
  LValue: string;
begin
  SiMain.LogDebug('parseLine: %s',[ALine]);
  LPos := Pos(' ',ALine);
  LKey := LeftStr(ALine, LPos-1);
  LValue := RightStr(ALine,Length(ALine)-LPos);

  if LKey = 'tree' then
    FTree := LValue
  else if LKey = 'parent' then
    addParent(LValue)
  else if LKey = 'author' then
    FAuthor.Parse(LValue)
  else if LKey = 'committer' then
    FCommitter.Parse(LValue);
end;

function TCRGitCommit.getParents(AIndex: Integer): string;
begin
  if (AIndex >= 0) and (AIndex < FParentsCount) then
    Result := FParents[AIndex]
  else
    raise EListError.CreateFmt('Parent index Out of bound (%d)',[AIndex]);
end;

class constructor TCRGitCommit.Create;
begin
  FAuxList := TStringList.Create;
end;

class destructor TCRGitCommit.Destroy;
begin
  FAuxList.Free;
end;

constructor TCRGitCommit.Create(ACommit: string;
    ACommitObj: TCRGitCommit = nil; ATree: string = '');
var
  I: Integer;
begin
  SiMain.LogVerbose('CreateCommitObj: %s',[ACommit]);
  FMessage := TStringList.Create;
  FCommit := ACommit;
  if ACommitObj <> nil then begin
    FMessage.Assign(ACommitObj.FMessage);
    FParentsCount := ACommitObj.FParentsCount;
    SetLength(FParents,FParentsCount);
    for I := 0 to FParentsCount - 1 do
      FParents[I] := ACommitObj.FParents[I];

    FAuthor := ACommitObj.FAuthor;
    FCommitter := ACommitObj.FCommitter;
    if ATree <> '' then
      FTree := ATree
    else
      FTree := ACommitObj.FTree;
  end;
end;

destructor TCRGitCommit.Destroy;
begin
  FMessage.Free;
end;

procedure TCRGitCommit.Parse(AFile: string);
var
  LString: string;
  LHeader: Boolean;
begin
  FAuxList.LoadFromFile(AFile);
  LHeader := true;
  for LString in FAuxList do begin
    if LHeader then
      if LString = '' then
        LHeader := false
      else
        parseLine(LString)
    else
      FMessage.Add(Lstring);
  end;
  // we put this at bottom bacouse we need parsed "Commit" string
  SiMain.LogStringList(lvDebug,Format('Commit data of %s',[Commit]),FAuxList);
end;

procedure TCRGitCommit.MessageToStream(AStream: TStringStream);
var
  LStr: string;
begin
  for LStr in FMessage do
      AStream.WriteString(LStr+#10);
end;

procedure TCRGitInterface.progressIndication(AStep: TCRGitProgressStep;
    APosition: Integer; AText: string = '');
begin
  if Assigned(FOnProgress) then
    FOnProgress(AStep,APosition,AText);
end;

function TCRGitInterface.executeCommand(ACmd: string; AOut: TPipeStream; AIn: TPipeStream = nil): Boolean;
begin
  Result := executeCommand(ACmd,'',AOut,AIn);
end;

function TCRGitInterface.executeCommand(ACmd: string; AFile: string = ''; AOut: TPipeStream = nil;
    AIn: TPipeStream = nil): Boolean;
var
  PLErrorString: PChar;
  LOutHandle: THandle;
  LFileMode: Cardinal;
  error: DWORD;
  success: Boolean;
  si: STARTUPINFO;
  pi: PROCESS_INFORMATION;
  sa: SECURITY_ATTRIBUTES;
begin
  // command do not ammit path in dos form
  ACmd := ReplaceStr(ACmd,'\','/');
  SiMain.EnterMethod(self,'executeCommand');
  ACmd := FCmdPath + ACmd;

  LOutHandle := 0;
  SiMain.LogDebug('CMD: execute cmd <' + ACmd + '>');
  FillChar(si,sizeof(STARTUPINFO),#0);
  FillChar(pi,sizeof(PROCESS_INFORMATION),#0);

  si.cb := sizeof(STARTUPINFO);
  GetStartupInfo(si);
  if AFile <> '' then begin
    sa.nLength := sizeof(SECURITY_ATTRIBUTES);
    sa.lpSecurityDescriptor := nil;
    sa.bInheritHandle := true;

    LOutHandle := CreateFile(
      PChar(AFile),
      GENERIC_WRITE,
      FILE_SHARE_READ or FILE_SHARE_WRITE,
      @sa,
      CREATE_ALWAYS,
      FILE_ATTRIBUTE_NORMAL,
      0);

    if LOutHandle = INVALID_HANDLE_VALUE then
      raise EGitError.Create('Unable to create temp file');

    si.hStdError := LOutHandle;
  end else if AOut <> nil then begin
    SetHandleInformation(AOut.ReadHandle, HANDLE_FLAG_INHERIT, 0);
    LOutHandle := AOut.WriteHandle;
  end;

  if (AIn <> nil) then begin
    SetHandleInformation(AIn.WriteHandle, HANDLE_FLAG_INHERIT, 0);
    si.hStdInput := AIn.ReadHandle;
    si.dwFlags    := STARTF_USESTDHANDLES
  end;

  if LOutHandle <> 0 then begin
    si.hStdOutput := LOutHandle;
    si.dwFlags    := STARTF_USESTDHANDLES
  end;

  si.wShowWindow := SW_HIDE;
  si.dwFlags := si.dwFlags or STARTF_USESHOWWINDOW;

  success := CreateProcess(
    nil,               // Application
    PChar(ACmd),       // Cmd line
    nil,               // Security descriptor = default
    nil,               // Thread descriptor = default
    true,              // Handle inheritance flag
    NORMAL_PRIORITY_CLASS,  // Creation flag
    nil,               // pointer to new environment block
		nil,               // current dir, same of the caller
		si,                // STARTUPINFO
		pi                 // PROCESSINFORMATION
    );

  if not success then begin
      error := GetLastError;
      FormatMessage(
          FORMAT_MESSAGE_ALLOCATE_BUFFER or  FORMAT_MESSAGE_FROM_SYSTEM,
          nil,
          error,
          MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), // Default language
          @PLErrorString,
          0,
          nil
      );
      SiMain.LogWarning('CMD: Fail %s',[PLErrorString]);
      LocalFree(Cardinal(PLErrorString));
      Result := false;
  end else begin
      SiMain.LogDebug('CMD: waiting termination...');
      if AIn <> nil then begin
        // if pipe has an input stream we connect to this
        AIn.Connect;

        // closein process close also stdin pipe to give EOF to child process
        // we close non inherited side of pipe
        AIn.CloseWrite;
      end;
      if AOut <> nil then
        AOut.CloseWrite;

      WaitForSingleObject(pi.hProcess,INFINITE);
      GetExitCodeProcess(pi.hProcess,error);
      if error <> 0 then begin
          Result := false;
          SiMain.LogWarning('CMD: Terminated False');
      end else begin
          Result := true;
          SiMain.LogDebug('CMD: Terminated True');
      end;
  end;
  CloseHandle(pi.hProcess);
  CloseHandle(pi.hThread);
  if AFile <> '' then begin
    CloseHandle(LOutHandle);
    if not Result then
      CopyFile(PChar(AFile),kErrorName,false);
  end;
  SiMain.LeaveMethod(self,'executeCommand');
end;

procedure TCRGitInterface.updateUserEnvVars(AId: string; var AReference: TCRGitCommitInfo; ANew: TCRGitCommitInfo);
begin
  if AReference.Name <> ANew.Name then begin
    AReference.Name := ANew.Name;
    SetEnvironmentVariable(PChar('GIT_'+AId+'_NAME'),PChar(AReference.Name));
  end;

  if AReference.EMail <> ANew.EMail then begin
    AReference.EMail := ANew.EMail;
    SetEnvironmentVariable(PChar('GIT_'+AId+'_EMAIL'),PChar(AReference.EMail));
  end;

  if AReference.Time <> ANew.Time then begin
    AReference.Time := ANew.Time;
    SetEnvironmentVariable(PChar('GIT_'+AId+'_DATE'),PChar(AReference.Time));
  end;
end;

procedure TCRGitInterface.findGitPathFormInno;
var
  LRegistry: TRegistry;
  LKeyValue: string;
begin
  LRegistry := TRegistry.Create(KEY_READ);
  LRegistry.RootKey := HKEY_LOCAL_MACHINE;
  try
    if LRegistry.OpenKey(kInnoKey,false) then begin
      LKeyValue := LRegistry.ReadString(kInnoValue);
      if LKeyValue = '' then
        raise EGitError.Create('No git installed (1)');

      FCmdPath := LKeyValue + 'bin\';
      SiMain.LogVerbose('GIT: read Cmdpath: <%s>',[FCmdPath])
    end else begin
      SiMain.LogWarning('No inno key present');
      raise EGitError.Create('No git installed (2)');
    end;
  finally
    LRegistry.Free;
  end;
end;

procedure TCRGitInterface.setIndexFilename(AName: string);
begin
  if AName <> FIndexFilename then begin
    FIndexFilename := AName;
    SetEnvironmentVariable('GIT_INDEX_FILE',PChar(ANAme));
  end;
end;

function TCRGitInterface.mapCommit(ACommitStr: string): string;
var
  LIdx: Integer;
begin
  Result := FCommitMapper.Values[ACommitStr];
  if Result = '' then
    Result := ACommitStr;
end;

function TCRGitInterface.createParentStr(ACommit: TCRGitCommit): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to ACommit.ParentsCount - 1 do
     Result := Result + ' -p ' + mapCommit(ACommit.Parents[I]);
end;

procedure TCRGitInterface.updateRefList;
var
  I: Integer;
  LHash: string;
  LNewHash: string;
  LRefName: string;
  LArray: TStringDynArray;

  procedure splitString(AString: string; var AHash,ARefName: string);
  var
    LPos: Integer;
begin
    LPos := Pos(' ',AString);
    AHash := LeftStr(AString,LPos-1);
    ARefName := RightStr(AString,Length(AString)-LPos);
  end;
begin
  // create reference list
  FRefList.Clear;
  executeCommand('git.exe show-ref',kTempName);
  FRefList.LoadFromFile(kTempName);

  // update reference if a commit hash is different
  for I := 0 to FRefList.Count - 1 do begin
    splitString(FRefList.Strings[I],LHash,LRefName);
    LNewHash := mapCommit(LHash);
    // if we have an update sha1 hash we update this
    if LHash <> LNewHash  then begin
      // LArray[1] is reference name
      SiMain.LogVerbose('Update ref %s new hash %s',[LRefName,LNewHash]);
      executeCommand(Format('git.exe update-ref %s %s',[LRefName,LNewHash]));
    end;
  end;
  DeleteFile(kTempName);
end;

function TCRGitInterface.refToHead(ARef: string): string;
var
  LArray: TStringDynArray;
begin
  Result := '';
  LArray := SplitString(ARef,'/');
  if Length(LArray) >= 2 then
    if LArray[Length(LArray)-2] = 'heads' then
      Result := LArray[Length(LArray)-1];
end;

procedure TCRGitInterface.updateIndex(APath: string);
var
  I: Integer;
  LPipeStream: TPipeStream;
begin
  SiMain.EnterMethod(self,'updateIndex');
  LPipeStream := nil;
  try
    SiMain.LogVerbose('Update path: commit: Path %s',[APath]);
    // get list of file in the index
    executeCommand('git.exe ls-files -s',kTempName);
    FAuxList.LoadFromFile(kTempName);
    for I := 0 to FAuxList.Count - 1 do
      // delphi way for <sed "s/\t/\t$(APath)\//">
      FAuxList.Strings[I] := ReplaceStr(FAuxList.Strings[I],#9,#9+APath+'/');
    // save modified string;
    //FAuxList.SaveToFile(kTempName);
    SiMain.LogDebug('Load file into pipe');

    // load file data into u
    LPipeStream := TPipeStream.Create;
    //LPipeStream.LoadFromFile(kTempName);
    LPipeStream.LoadFromStrings(FAuxList);

    SetEnvironmentVariable('GIT_INDEX_FILE',kNewIndexName);

    executeCommand('git.exe update-index --index-info',nil,LPipeStream);
    DeleteFile(kIndexName);
    RenameFile(kNewIndexName,kIndexName);
    DeleteFile(kTempName);
  finally
    SetEnvironmentVariable('GIT_INDEX_FILE',kIndexName);
    LPipeStream.Free;
    SiMain.LeaveMethod(self,'updateIndex');
  end;
end;

procedure TCRGitInterface.createCommitList;
var
  LCommit: TCRGitCommit;
  LCommitHash: string;
  LCount: Integer;
begin
  SiMain.LogVerbose('Create Commit List');
  executeCommand('git.exe rev-list --reverse --topo-order --all',kTempName);
  FAuxList.LoadFromFile(kTempName);
  FCommitList.Clear;

  progressIndication(kCount,FAuxList.Count,'Create commit tree');

  for LCount := 0 to FAuxList.Count - 1 do begin
    LCommitHash := FAuxList.Strings[LCount];
    LCommit := TCRGitCommit.Create(LCommitHash);
    executeCommand(Format('git.exe cat-file commit %s',[LCommitHash]),kTempName + '.commit');
    LCommit.Parse(kTempName + '.commit');
    FCommitList.Add(LCommit);
    progressIndication(kPosition,LCount+1);
  end;
  DeleteFile(PChar(kTempName + '.commit'));
end;

constructor TCRGitInterface.Create;
begin
  // find git path from uninstall key of InnoSetup
  findGitPathFormInno;
  FAuxList    := TStringList.Create;
  // unix domain so we forget #13 #10 (Aka CR LF)
  FAuxList.LineBreak := #10;
  FRefList   := TStringList.Create;
  FCommitMapper := TStringList.Create;
  FCommitList := TObjectList<TCRGitCommit>.Create;
end;

destructor TCRGitInterface.Destroy;
begin
  FCommitList.Free;
  FCommitMapper.Free;
  FRefList.Free;
  FAuxList.Free;
end;

procedure TCRGitInterface.Clear;
begin
  FCommitList.Clear;
  FCommitMapper.Clear;
  FRefList.Clear;
  FAuxList.Clear;
end;

procedure TCRGitInterface.List(AList: TStringList);
var
  LPos: Integer;
  LFile1,
  LFile: string;
begin
  executeCommand('git.exe ls-files',kTempName);
  FAuxList.Clear;
  FAuxList.LoadFromFile(kTempName);
  for LFile in FAuxList do begin
    LPos := Pos('/',LFile);
    if LPos > 0 then
      LFile1 := LeftStr(LFile,LPos-1)
    else
      LFile1 := LFile;
    if AList.IndexOf(LFile1) < 0 then
      AList.Add(LFile1);
  end;
  DeleteFile(kTempName);
end;

procedure TCRGitInterface.Commit(AMessage: string);
begin
  executeCommand(Format('git.exe commit -m "%s"',[AMessage]));
end;

procedure TCRGitInterface.MergeRepo(ARepo: string);
var
  LHead: string;
  LRef: string;
  LRefList: TStringDynArray;
begin
  executeCommand(Format('git.exe remote add origin %s',[ARepo]));
  // we pull master branch
  executeCommand('git.exe pull origin master');

  // we get other branches
  executeCommand('git.exe fetch origin');
  // now we fetch also tags objects
  executeCommand('git.exe fetch --tags origin');

  // create local branch that track remotes one
  // we skip master that already exists
  for LRef in FRefList do begin
    LHead := refToHead(LRef);
    if (LHead <> '') and (LHead <> 'master') then
      executeCommand(Format('git.exe branch %s origin/%s',[LHead,LHead]));
  end;

  // remove origin
  executeCommand('git.exe remote rm origin');
end;

procedure TCRGitInterface.InitMerge;
begin
  executeCommand('git.exe init');
  CopyFile(PChar(kFileConfig),PChar(kFileConfig+'.backup'),false);
end;

procedure TCRGitInterface.FinalizeMerge;
begin
  CopyFile(PChar(kFileConfig+'.backup'),PChar(kFileConfig),false);
  DeleteFile(PChar(kFileConfig+'.backup'));
end;

procedure TCRGitInterface.UpdateCommitTree(APath: string);
var
  LNewCommit,
  LCommit: TCRGitCommit;
  LTree,
  LCommitStr,
  LParentStr: string;
  I: Integer;
  LCommitIdx: Integer;
  LOutPipe: TPipeStream;
  LInPipe: TPipeStream;
  LStrStream: TStringStream;
  LCommitterInfo: TCRGitCommitInfo;
  LAuthorInfo: TCRGitCommitInfo;
begin
  // clear last result (this is mainform entrypoint)
  Clear;
  // create main commit list
  createCommitList;
  progressIndication(kCount,FCommitList.Count,'Rewite commit tree');

  LInPipe := nil;
  LOutPipe := TPipeStream.Create;
  for I := 0 to FCommitList.Count - 1 do begin
    // load tree
    LCommit := FCommitList.Items[I];
    SiMain.LogVerbose('Process Commit: %s',[LCommit.Commit]);
    executeCommand(Format('git.exe read-tree %s',[LCommit.Tree]));
    // update tree
    updateIndex(APath);
    LOutPipe.Reset;
    executeCommand('git.exe write-tree',LOutPipe);
    // commit new tree
    LTree := Trim(LOutPipe.ReadString);
    SiMain.LogVerbose('New tree: %s',[LTree]);
    if LTree <> '' then begin
      // create string of parent of this commmit
      LParentStr := createParentStr(LCommit);
      // commit a tree
      try
        LInPipe := TPipeStream.Create;
        // TODO 2 -cFIXME : skip TStringStream
        LStrStream := TStringStream.Create;
        LCommit.MessageToStream(LStrStream);
        LStrStream.Seek(0,soBeginning);
        LInPipe.CopyFrom(LStrStream,LStrStream.Size);
        LOutPipe.Reset;
        updateUserEnvVars('COMMITTER',LCommitterInfo,LCommit.Committer);
        updateUserEnvVars('AUTHOR',LAuthorInfo,LCommit.Author);

        executeCommand(Format('git.exe commit-tree %s %s',[LTree,LParentStr]),LOutPipe,LInPipe);
        LCommitStr := Trim(LOutPipe.ReadString);
        SiMain.LogVerbose('New commit: %s->%s',[LCommit.Commit,LCommitStr]);
      finally
        FreeAndNil(LInPipe);
        FreeAndNil(LStrStream);
      end;
      FCommitMapper.Values[LCommit.Commit] := LCommitStr;
    end;
    progressIndication(kPosition,I+1);
  end;
  updateRefList;
end;

end.
