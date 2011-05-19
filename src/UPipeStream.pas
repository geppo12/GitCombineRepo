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

unit UPipeStream;

interface

uses
  Classes,
  Windows;

type
  TPipeStream = class(TStream)
    private
    FReadHandle: THandle;
    FWriteHandle: THandle;

    public
    constructor Create;
    destructor Destroy; override;
    procedure Open;
    procedure CloseWrite;
    procedure CloseRead;
    procedure Reset;
    function ReadString: string;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    procedure LoadFromFile(AFile: string);
    //* windows readHandle: not valid after ReOpen
    property ReadHandle: THandle read FReadHandle;
    //* windows writeHandle: not valid after ReOpen
    property WriteHandle: THandle read FWriteHandle;
  end;

implementation

uses
  SysUtils;

constructor TPipeStream.Create;
begin
  inherited;
  Open;
end;

destructor TPipeStream.Destroy;
begin
  CloseRead;
  CloseWrite;
  inherited;
end;

procedure TPipeStream.Open;
var
  LSecurityAttr: SECURITY_ATTRIBUTES;
begin
  LSecurityAttr.nLength := SizeOf(SECURITY_ATTRIBUTES);
  LSecurityAttr.lpSecurityDescriptor := nil;
  LSecurityAttr.bInheritHandle := True;
  CreatePipe(FReadHandle,FWriteHandle,@LSecurityAttr,0);
end;

procedure TPipeStream.CloseWrite;
begin
  if FWriteHandle <> INVALID_HANDLE_VALUE then
    CloseHandle(FWriteHandle);
  FWriteHandle := INVALID_HANDLE_VALUE;
end;

procedure TPipeStream.CloseRead;
begin
  if FReadHandle <> INVALID_HANDLE_VALUE then
    CloseHandle(FReadHandle);
  FReadHandle := INVALID_HANDLE_VALUE;
end;

procedure TPipeStream.Reset;
begin
  CloseRead;
  CloseWrite;
  Open;
end;

function TPipeStream.ReadString: string;
const
  kBufSize = 32768;
var
  LPos: Integer;
  LRed: Integer;
  LBuf: array [0..kBufSize-1] of AnsiChar;
begin
  Result := '';
  LRed := Read(LBuf,kBufSize);
  LBuf[LRed] := #0;
  Result := LBuf;
end;

function TPipeStream.Read(var Buffer; Count: Longint): Longint;
begin
  if not ReadFile(FReadHandle,Buffer,Count,Cardinal(Result),nil) then
    Result := -1;
end;

function TPipeStream.Write(const Buffer; Count: Longint): Longint;
begin
  if not WriteFile(FWriteHandle,Buffer,Count,Cardinal(Result),nil) then
    Result := -1;
end;

procedure TPipeStream.LoadFromFile(AFile: string);
var
  LFileStream: TFileStream;
begin
  LFileStream := TFileStream.Create(AFile,fmOpenRead);
  try
    CopyFrom(LFileStream,LFileStream.Size);
  finally
    LFileStream.Free;
  end;
end;



end.
