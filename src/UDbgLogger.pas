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
{ engine agnostic logger. NOT THREAD SAFE }
unit UDbgLogger;

interface

uses
  SysUtils;

type
  TDbgLogClass = (
    lcInternal,
    lcFatal,
    lcError,
    lcWarning,
    lcMessage,
    lcVerbose,
    lcDebug
  );

  TDbgLoggerEngine = class abstract
    private
    FEnable: Boolean;
    procedure setEnable(AEnable: Boolean);

    protected
    FInitialized: Boolean;
    procedure enableEngine(AEnable: Boolean); virtual;

    public
    procedure Init; virtual;
    procedure Reset; virtual;
    procedure LogData(ALogLevel: TDbgLogClass; AString: string); virtual; abstract;
    procedure LogException(AString: string = ''); virtual;
    procedure EnterMethod(AObject: TObject; AMethodName: string); virtual;
    procedure LeaveMethod(AObject: TObject; AMethodName: string); virtual;
    property Enable: Boolean read FEnable write setEnable;
  end;

  TDbgLoggerType = (
    leWindows,
    leSmartInspect,
    leCodeSite
  );
  TDbgLogger = class
    class var FInstance: TDbgLogger;
    var
    FEnable: Boolean;
    FLoggerEngine: TDbgLoggerEngine;

    procedure setEnable(AEnable: Boolean);
    public
    class constructor Create;
    class destructor Destroy;
    destructor Destroy; override;
    procedure InitEngine(AEngine: TDbgLoggerType);
    procedure Reset;
    procedure LogException(AString: string = '');
    procedure LogError(AString: string); overload;
    procedure LogError(AStringFmt: string; AArgs: array of const); overload;
    procedure LogWarning(AString: string); overload;
    procedure LogWarning(AStringFmt: string; AArgs: array of const); overload;
    procedure LogMessage(AString: string); overload;
    procedure LogMessage(AStringFmt: string; AArgs: array of const); overload;
    procedure LogVerbose(AString: string); overload;
    procedure LogVerbose(AStringFmt: string; AArgs: array of const); overload;
    procedure LogDebug(AString: string); overload;
    procedure LogDebug(AStringFmt: string; AArgs: array of const); overload;
    procedure EnterMethod(AObject: TObject; AMethodName: string);
    procedure LeaveMethod(AObject: TObject; AMethodName: string);
    property Enable: boolean read FEnable write setEnable;

    class property Instance: TDbgLogger read FInstance;
  end;

  ELogEngineError = class(Exception);
  ELogEngineUnavaible = class(ELogEngineError)
    public
    constructor Create(AType: TDbgLoggerType);
  end;

implementation

uses
{$IFDEF USE_SMARTINSPECT}
  SiAuto,
  SmartInspect,
{$ENDIF}
  TypInfo,
  Windows,
  Forms;

type
{$REGION 'Engine SmartInspact'}
{$IFDEF USE_SMARTINSPECT}
  TDbgLoggerEngineSI = class(TDbgLoggerEngine)
    protected
    procedure enableEngine(AEnabled: Boolean); override;

    public
    procedure Init; override;
    procedure Reset; override;
    procedure LogData(AClass: TDbgLogClass; AString: string); override;
    procedure LogException(AString: string = ''); override;
    procedure EnterMethod(AObject: TObject; AMethodName: string); override;
    procedure LeaveMethod(AObject: TObject; AMethodName: string); override;
  end;
{$ENDIF}
{$ENDREGION}

{$REGION 'Engine Windows'}
  TDbgLoggerEngineWin = class(TDbgLoggerEngine)
    public
    procedure LogData(AClass: TDbgLogClass; AString: string); override;
  end;

{$ENDREGION}

// trick to support real application folder in future
function GetAppDataFolder: string;
begin
  Result := ExtractFilePath(Application.ExeName);
end;

{$REGION 'ELogEngineUnavaible'}
constructor ELogEngineUnavaible.Create(AType: TDbgLoggerType);
begin
  inherited Create(GetEnumName(TypeInfo(TDbgLoggerType),Ord(AType)))
end;


{$REGION 'TDbgLoggerEngine'}

procedure TDbgLoggerEngine.setEnable(AEnable: Boolean);
begin
  // if engine not inizialized we suppose that is not enebled
  if FInitialized then begin
    if FEnable <> AEnable then begin
      FEnable := AEnable;
      enableEngine(AEnable);
    end;
  end;
end;

procedure TDbgLoggerEngine.enableEngine(AEnable: Boolean);
begin
  // can be redefinited form derived classes
end;

procedure TDbgLoggerEngine.Init;
begin
  FInitialized := true;
  // can be redefinited form derived classes
end;

procedure TDbgLoggerEngine.Reset;
begin
  // can be redefinited form derived classes
end;

procedure TDbgLoggerEngine.LogException(AString: string = '');
begin
  LogData(lcError,Format('EXCEPTION: %s',[AString]));
end;

procedure TDbgLoggerEngine.EnterMethod(AObject: TObject; AMethodName: string);
begin
  LogData(lcDebug,Format('>>>ENTER %s.%s',[AObject.ClassName,AMethodName]));
end;

procedure TDbgLoggerEngine.LeaveMethod(AObject: TObject; AMethodName: string);
begin
  LogData(lcDebug,Format('<<<LEAVE %s.%s',[AObject.ClassName,AMethodName]));
end;

{$ENDREGION}

{$REGION 'TDbgLogger'}

procedure TDbgLogger.setEnable(AEnable: Boolean);
begin
  if FEnable <> AEnable then begin
    if FLoggerEngine <> nil then begin
      FEnable := AEnable;
      FLoggerEngine.Enable := AEnable;
    end else
      FEnable := false;
  end;
end;

class constructor TDbgLogger.Create;
begin
  FInstance := TDbgLogger.Create;
end;

class destructor TDbgLogger.Destroy;
begin
  FInstance.Free;
end;

destructor TDbgLogger.Destroy;
begin
  FLoggerEngine.Free;
end;

procedure TDbgLogger.InitEngine(AEngine: TDbgLoggerType);
begin
  FLoggerEngine.Free;
  FLoggerEngine := nil;

  case AEngine of
    leWindows: FLoggerEngine := TDbgLoggerEngineWin.Create;
    leSmartInspect:
{$IFDEF USE_SMARTINSPECT}
      FLoggerEngine := TDbgLoggerEngineSI.Create;
{$ELSE}
      raise ELogEngineUnavaible.Create(leSmartInspect);
{$ENDIF}

    // TODO 1 -cFEATURE : implement codesite driver
    leCodeSite: raise ELogEngineUnavaible.Create(leSmartInspect);
    else
      raise ELogEngineError.Create('Engine required unknown');
  end;
  if FLoggerEngine <> nil then begin
    FLoggerEngine.Init;
    FEnable := FLoggerEngine.Enable;
  end else
    FEnable := false;
end;

procedure TDbgLogger.Reset;
begin
  if FEnable then
    FLoggerEngine.Reset;
end;


procedure TDbgLogger.LogException(AString: string);
begin
  if FEnable then try
    FLoggerEngine.LogException(AString);
  except
    on E: Exception do
      FLoggerEngine.LogData(lcInternal,E.Message);
  end;
end;

procedure TDbgLogger.LogError(AString: string);
begin
  if FEnable then try
    FLoggerEngine.LogData(lcError,AString);
  except
    on E: Exception do
      FLoggerEngine.LogData(lcInternal,E.Message);
  end;
end;

procedure TDbgLogger.LogError(AStringFmt: string; AArgs: array of const);
begin
  if FEnable then try
    FLoggerEngine.LogData(lcError,Format(AStringFmt,AArgs));
  except
    on E: Exception do
      FLoggerEngine.LogData(lcInternal,E.Message);
  end;
end;

procedure TDbgLogger.LogWarning(AString: string);
begin
  if FEnable then try
    FLoggerEngine.LogData(lcWarning,AString);
  except
    on E: Exception do
      FLoggerEngine.LogData(lcInternal,E.Message);
  end;
end;

procedure TDbgLogger.LogWarning(AStringFmt: string; AArgs: array of const);
begin
  if FEnable then try
    FLoggerEngine.LogData(lcWarning,Format(AStringFmt,AArgs));
  except
    on E: Exception do
      FLoggerEngine.LogData(lcInternal,E.Message);
  end;
end;

procedure TDbgLogger.LogMessage(AString: string);
begin
  if FEnable then try
    FLoggerEngine.LogData(lcMessage,AString);
  except
    on E: Exception do
      FLoggerEngine.LogData(lcInternal,E.Message);
  end;
end;

procedure TDbgLogger.LogMessage(AStringFmt: string; AArgs: array of const);
begin
  if FEnable then try
    FLoggerEngine.LogData(lcMessage,Format(AStringFmt,AArgs));
  except
    on E: Exception do
      FLoggerEngine.LogData(lcInternal,E.Message);
  end;
end;

procedure TDbgLogger.LogVerbose(AString: string);
begin
  if FEnable then try
    FLoggerEngine.LogData(lcVerbose,AString);
  except
    on E: Exception do
      FLoggerEngine.LogData(lcInternal,E.Message);
  end;
end;

procedure TDbgLogger.LogVerbose(AStringFmt: string; AArgs: array of const);
begin
  if FEnable then try
    FLoggerEngine.LogData(lcVerbose,Format(AStringFmt,AArgs));
  except
    on E: Exception do
      FLoggerEngine.LogData(lcInternal,E.Message);
  end;
end;

procedure TDbgLogger.LogDebug(AString: string);
begin
  if FEnable then try
    FLoggerEngine.LogData(lcDebug,AString);
  except
    on E: Exception do
      FLoggerEngine.LogData(lcInternal,E.Message);
  end;
end;

procedure TDbgLogger.LogDebug(AStringFmt: string; AArgs: array of const);
begin
  if FEnable then try
    FLoggerEngine.LogData(lcDebug,Format(AStringFmt,AArgs));
  except
    on E: Exception do
      FLoggerEngine.LogData(lcInternal,E.Message);
  end;
end;

procedure TDbgLogger.EnterMethod(AObject: TObject; AMethodName: string);
begin
  if FEnable then try
    FLoggerEngine.EnterMethod(AObject,AMethodName);
  except
    on E: Exception do
      FLoggerEngine.LogData(lcInternal,E.Message);
  end;
end;

procedure TDbgLogger.LeaveMethod(AObject: TObject; AMethodName: string);
begin
  if FEnable then try
    FLoggerEngine.LeaveMethod(AObject,AMethodName);
  except
    on E: Exception do
      FLoggerEngine.LogData(lcInternal,E.Message);
  end;
end;


{$ENDREGION}

{$REGION 'TDbgLoggerEngineSI'}
{$IFDEF USE_SMARTINSPECT}

procedure TDbgLoggerEngineSI.enableEngine(AEnabled: Boolean);
begin
  Si.Enabled := AEnabled;
end;

// TODO 2 -cLANGUAGE : tradurre in inglese i commenti
procedure TDbgLoggerEngineSI.Init;
var
  LFileNameSic: string;
  LFileNameSil: string;
  LUserDir: string;
  sicDone: Boolean;
begin
  FInitialized := false;
  { suppongo che il file *.sic esita }
  sicDone := true;
  LUserDir := GetAppDataFolder;

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
    { carico la configurazione dal file sic }
    Si.LoadConfiguration(LFileNameSic);
{$IFDEF _MICROSEC}
    Si.Resolution := crHigh;
{$ENDIF}
    SiMain.ClearAll;

    SiMain.LogVerbose('Read SmartInspect config %s',[LFileNameSic]);
{$IFNDEF EUREKALOG}
    Application.OnException := SiMain.ExceptionHandler;
{$ENDIF}
    FInitialized := true;
    // for consistency with Engine enable flag
    // TODO 1 -cFIXME : review this point
    Si.Enabled := false;
  end
{$IFDEF NO_SIC_NEEDED}
  else begin
    Si.Connections := 'pipe()';
    Si.Level := lvDebug;
    Si.DefaultLevel := lvDebug;
    FInitialized := true;
    Si.Enabled := true;
    SiMain.ClearAll;
    SiMain.LogVerbose('Read SmartInspect config %s',[LFileNameSic]);
{$IFNDEF EUREKALOG}
    Application.OnException := SiMain.ExceptionHandler;
{$ENDIF}
    // for consistency with Engine enable flag
    // TODO 1 -cFIXME : review this point
    Si.Enabled := false;
  end
{$ENDIF};
end;

procedure TDbgLoggerEngineSI.Reset;
begin
  SiMain.ClearAll;
end;

procedure TDbgLoggerEngineSI.LogData(AClass: TDbgLogClass; AString: string);
begin
  case AClass of
    lcInternal: SiMain.LogFatal('Internal [TDbgLoggerEngineSI] error: %s',[AString]);
    lcFatal: SiMain.LogFatal(AString);
    lcError: SiMain.LogError(AString);
    lcWarning: SiMain.LogWarning(AString);
    lcMessage: SiMain.LogMessage(AString);
    lcVerbose: SiMain.LogVerbose(AString);
    lcDebug: SiMain.LogDebug(AString);
  end;
end;

procedure TDbgLoggerEngineSI.LogException(AString: string);
begin
  SiMain.LogException(AString);
end;

procedure TDbgLoggerEngineSI.EnterMethod(AObject: TObject; AMethodName: string);
begin
  SiMain.EnterMethod(Aobject,AMethodName);
end;

procedure TDbgLoggerEngineSI.LeaveMethod(AObject: TObject; AMethodName: string);
begin
  SiMain.LeaveMethod(Aobject,AMethodName);
end;

{$ENDIF} { USE_SMARTINSPECT }
{$ENDREGION}


{$REGION 'TDbgLoggerWin'}
procedure TDbgLoggerEngineWin.LogData(AClass: TDbgLogClass; AString: string);
var
  LHdrString: string;
begin
  case AClass of
    lcInternal: LHdrString := '[Internal Error]';
    lcFatal: LHdrString := '[Fatal]';
    lcError: LHdrString := '[Error]';
    lcWarning: LHdrString := '[Warning]';
    lcMessage: LHdrString := '[Message]';
    lcVerbose: LHdrString := '[Verbose]';
    lcDebug: LHdrString := '[Debug]';
  end;

  OutputDebugString(PChar('>>> '+ LHdrString + ' ' + AString + ' *** '));

end;

end.
