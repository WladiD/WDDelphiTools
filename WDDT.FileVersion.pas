unit WDDT.FileVersion;

interface

uses
  Winapi.Windows,
  Winapi.ImageHlp,
  System.SysUtils,
  System.DateUtils;

type
  {**
   * Efficient representation (needs only 24 bytes) of file and product version
   * without a object and the need of freeing them.
   *}
  TFileVersion = record
  private
    {**
     * Bit      Value
     * 63..48    Major
     * 47..32    Minor
     * 31..16    Release
     * 15..0     Build
     *}
    FFileVersion64: Int64;
    FProductVersion64: Int64;

    FBuildDateTime: TDateTime;

    function GetFileVersion32: Integer;

    function GetFileMajor: Integer;
    procedure SetFileMajor(Major: Integer);

    function GetFileMinor: Integer;
    procedure SetFileMinor(Minor: Integer);

    function GetFileRelease: Integer;
    procedure SetFileRelease(Release: Integer);

    function GetFileBuild: Integer;
    procedure SetFileBuild(Build: Integer);

    function GetProductVersion32: Integer;

    function GetProductMajor: Integer;
    procedure SetProductMajor(Major: Integer);

    function GetProductMinor: Integer;
    procedure SetProductMinor(Minor: Integer);

    function GetProductRelease: Integer;
    procedure SetProductRelease(Release: Integer);

    function GetProductBuild: Integer;
    procedure SetProductBuild(Build: Integer);
  public
    constructor Create(const FileName: TFileName); overload;
    constructor Create(FileVersion64: Int64; ProductVersion64: Int64;
      BuildDateTime: TDateTime); overload;

    class function GetVersion32(const Version64: Int64): Integer; static;

    class function GetMajor(const Version64: Int64): Integer; static;
    class procedure SetMajor(var Version64: Int64; Major: Integer); static;

    class function GetMinor(const Version64: Int64): Integer; static;
    class procedure SetMinor(var Version64: Int64; Minor: Integer); static;

    class function GetRelease(const Version64: Int64): Integer; static;
    class procedure SetRelease(var Version64: Int64; Release: Integer); static;

    class function GetBuild(const Version64: Int64): Integer; static;
    class procedure SetBuild(var Version64: Int64; Build: Integer); static;

    class function Compare(const LeftVersion64, RightVersion64: Int64): Integer; static;
    class function CompareFileVersion(
      const LeftVersion, RightVersion: TFileVersion): Integer; static;
    class function CompareProductVersion(
      const LeftVersion, RightVersion: TFileVersion): Integer; static;

    class function ToString(const Version64: Int64; IncludeBuild, IncludeRelease,
      IncludeMinor: Boolean): string; static;

    function FileVersionToString(IncludeBuild: Boolean = False; IncludeRelease: Boolean = True;
      IncludeMinor: Boolean = True): string;

    function ProductVersionToString(IncludeBuild: Boolean = False; IncludeRelease: Boolean = True;
      IncludeMinor: Boolean = True): string;

    property FileVersion32: Integer read GetFileVersion32;
    property FileMajor: Integer read GetFileMajor write SetFileMajor;
    property FileMinor: Integer read GetFileMinor write SetFileMinor;
    property FileRelease: Integer read GetFileRelease write SetFileRelease;
    property FileBuild: Integer read GetFileBuild write SetFileBuild;

    property ProductVersion32: Integer read GetProductVersion32;
    property ProductMajor: Integer read GetProductMajor write SetProductMajor;
    property ProductMinor: Integer read GetProductMinor write SetProductMinor;
    property ProductRelease: Integer read GetProductRelease write SetProductRelease;
    property ProductBuild: Integer read GetProductBuild write SetProductBuild;

    property BuildDateTime: TDateTime read FBuildDateTime;
  end;

implementation

{ TFileVersion }

constructor TFileVersion.Create(const FileName: TFileName);
var
  Size, Size2: DWord;
  Pt: Pointer;
  Info: ^TVSFixedFileInfo;
  FileTime: TFILETIME;
  SystemTime: TSYSTEMTIME;

  function GetBuildDatePE: TDateTime;
  var
    LI: TLoadedImage;
    FileNameAnsi: AnsiString;
  begin
    FileNameAnsi := AnsiString(FileName);
    if MapAndLoad(PAnsiChar(FileNameAnsi), nil, @LI, False, True) then
    begin
      Result := LI.FileHeader.FileHeader.TimeDateStamp / SecsPerDay + UnixDateDelta;
      Result := TTimeZone.Local.ToLocalTime(Result);
      UnMapAndLoad(@LI);
    end
    else
      Result := 0;
  end;

begin
  FFileVersion64 := 0;
  FProductVersion64 := 0;
  FBuildDateTime := 0;
  if FileName = '' then
    Exit;
  // GetFileVersionInfo modifies the filename parameter data while parsing.
  // Copy the string const into a local variable to create a writeable copy.
  Size := GetFileVersionInfoSize(Pointer(@FileName[1]), Size2);
  if Size > 0 then
  begin
    GetMem(Pt, Size);
    try
      GetFileVersionInfo(Pointer(FileName), 0, Size, Pt);
      VerQueryValue(Pt, '\', Pointer(Info), Size2);
      with Info^ do
      begin
        FFileVersion64 := (Int64(dwFileVersionMS) shl 32) or dwFileVersionLS;
        FProductVersion64 := (Int64(dwProductVersionMS) shl 32) or dwProductVersionLS;

        if (dwFileDateLS <> 0) and (dwFileDateMS <> 0) then
        begin
          FileTime.dwLowDateTime := dwFileDateLS; // built date from version info
          FileTime.dwHighDateTime := dwFileDateMS;
          FileTimeToSystemTime(FileTime, SystemTime);
          FBuildDateTime := EncodeDate(SystemTime.wYear, SystemTime.wMonth, SystemTime.wDay);
        end;
      end;
    finally
      FreeMem(Pt);
    end;
  end;

  // Next method to get the build date, if VerQueryValue don't return it
  if FBuildDateTime = 0 then
    FBuildDateTime := GetBuildDatePE;
end;

constructor TFileVersion.Create(FileVersion64, ProductVersion64: Int64; BuildDateTime: TDateTime);
begin
  FFileVersion64 := FileVersion64;
  FProductVersion64 := ProductVersion64;
  FBuildDateTime := BuildDateTime;
end;

{**
 * Contains major, minor and release version
 *
 * Bit     Value
 * 31..16  Major as Word
 * 15..8   Minor as Byte
 * 7..0    Release as Byte
 *}
class function TFileVersion.GetVersion32(const Version64: Int64): Integer;
begin
  Result := (GetMajor(Version64) shl 16) + (GetMinor(Version64) shl 8) + GetRelease(Version64);
end;

class function TFileVersion.GetMajor(const Version64: Int64): Integer;
begin
  Result := Version64 shr 48;
end;

class procedure TFileVersion.SetMajor(var Version64: Int64; Major: Integer);
begin
  Version64 := (Version64 and $0000FFFFFFFFFFFF) or (Int64(Major and $FFFF) shl 48);
end;

class function TFileVersion.GetMinor(const Version64: Int64): Integer;
begin
  Result := (Version64 shr 32) and $FFFF;
end;

class procedure TFileVersion.SetMinor(var Version64: Int64; Minor: Integer);
begin
  Version64 := (Version64 and $FFFF0000FFFFFFFF) or (Int64(Minor and $FFFF) shl 32);
end;

class function TFileVersion.GetRelease(const Version64: Int64): Integer;
begin
  Result := (Version64 shr 16) and $FFFF;
end;

class procedure TFileVersion.SetRelease(var Version64: Int64; Release: Integer);
begin
  Version64 := (Version64 and $FFFFFFFF0000FFFF) or (Int64(Release and $FFFF) shl 16);
end;

class function TFileVersion.GetBuild(const Version64: Int64): Integer;
begin
  Result := Version64 and $FFFF;
end;

class procedure TFileVersion.SetBuild(var Version64: Int64; Build: Integer);
begin
  Version64 := (Version64 and $FFFFFFFFFFFF0000) or Int64(Build and $FFFF);
end;

class function TFileVersion.ToString(const Version64: Int64; IncludeBuild, IncludeRelease,
  IncludeMinor: Boolean): string;
begin
  if IncludeMinor and IncludeRelease and IncludeBuild then
    Result := Format('%d.%d.%d.%d', [GetMajor(Version64), GetMinor(Version64), GetRelease((Version64)),
      GetBuild(Version64)])
  else if IncludeMinor and IncludeRelease then
    Result := Format('%d.%d.%d', [GetMajor((Version64)), GetMinor((Version64)), GetRelease(Version64)])
  else if IncludeMinor then
    Result := Format('%d.%d', [GetMajor(Version64), GetMinor(Version64)])
  else
    Result := IntToStr(GetMajor(Version64));
end;

{**
 * @return Result is > 0, if LeftVersion64 is higher than RightVersion64
 *         Result is < 0, if LeftVersion64 is lower than RightVersion64
 *         Result is = 0, if LeftVersion64 is equal to RightVersion64
 *}
class function TFileVersion.Compare(const LeftVersion64, RightVersion64: Int64): Integer;
begin
  Result := 
    ((GetMajor(LeftVersion64) - GetMajor(RightVersion64)) * 10000) +
    ((GetMinor(LeftVersion64) - GetMinor(RightVersion64)) * 1000) +
    ((GetRelease(LeftVersion64) - GetRelease(RightVersion64)) * 100) +
    (GetBuild(LeftVersion64) - GetBuild(RightVersion64));
end;

class function TFileVersion.CompareFileVersion(
  const LeftVersion, RightVersion: TFileVersion): Integer;
begin
  Result := Compare(LeftVersion.FFileVersion64, RightVersion.FFileVersion64);
end;

class function TFileVersion.CompareProductVersion(
  const LeftVersion, RightVersion: TFileVersion): Integer;
begin
  Result := Compare(LeftVersion.FProductVersion64, RightVersion.FProductVersion64);
end;

function TFileVersion.GetFileVersion32: Integer;
begin
  Result := GetVersion32(FFileVersion64);
end;

function TFileVersion.GetFileMajor: Integer;
begin
  Result := GetMajor(FFileVersion64);
end;

function TFileVersion.GetFileMinor: Integer;
begin
  Result := GetMinor(FFileVersion64);
end;

function TFileVersion.GetFileRelease: Integer;
begin
  Result := GetRelease(FFileVersion64);
end;

function TFileVersion.GetFileBuild: Integer;
begin
  Result := GetBuild(FFileVersion64);
end;

function TFileVersion.FileVersionToString(IncludeBuild, IncludeRelease, IncludeMinor: Boolean): string;
begin
  Result := ToString(FFileVersion64, IncludeBuild, IncludeRelease, IncludeMinor);
end;

function TFileVersion.GetProductVersion32: Integer;
begin
  Result := GetVersion32(FProductVersion64);
end;

function TFileVersion.GetProductMajor: Integer;
begin
  Result := GetMajor(FProductVersion64);
end;

function TFileVersion.GetProductMinor: Integer;
begin
  Result := GetMinor(FProductVersion64);
end;

function TFileVersion.GetProductRelease: Integer;
begin
  Result := GetRelease(FProductVersion64);
end;

function TFileVersion.GetProductBuild: Integer;
begin
  Result := GetBuild(FProductVersion64);
end;

function TFileVersion.ProductVersionToString(IncludeBuild, IncludeRelease,
  IncludeMinor: Boolean): string;
begin
  Result := ToString(FProductVersion64, IncludeBuild, IncludeRelease, IncludeMinor);
end;

procedure TFileVersion.SetFileBuild(Build: Integer);
begin
  SetBuild(FFileVersion64, Build);
end;

procedure TFileVersion.SetFileMajor(Major: Integer);
begin
  SetMajor(FFileVersion64, Major);
end;

procedure TFileVersion.SetFileMinor(Minor: Integer);
begin
  SetMinor(FFileVersion64, Minor);
end;

procedure TFileVersion.SetFileRelease(Release: Integer);
begin
  SetRelease(FFileVersion64, Release);
end;

procedure TFileVersion.SetProductBuild(Build: Integer);
begin
  SetBuild(FProductVersion64, Build);
end;

procedure TFileVersion.SetProductMajor(Major: Integer);
begin
  SetMajor(FProductVersion64, Major);
end;

procedure TFileVersion.SetProductMinor(Minor: Integer);
begin
  SetMinor(FProductVersion64, Minor);
end;

procedure TFileVersion.SetProductRelease(Release: Integer);
begin
  SetRelease(FProductVersion64, Release);
end;

end.
