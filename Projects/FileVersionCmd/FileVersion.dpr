program FileVersion;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.DateUtils,
  WDDT.FileVersion in '..\..\WDDT.FileVersion.pas';

procedure ShowUsage;
begin
  Writeln('Usage: ' + ExtractFileName(ParamStr(0)) + ' file [format]');
  Writeln;
  Writeln('format can hold following placeholder:');
  Writeln(' %FileMajor% %FileMinor% %FileRelease% %FileBuild% %FileVersion32%');
  Writeln(' %ProductMajor% %ProductMinor% %ProductRelease% %ProductBuild% %ProductVersion32%');
  Writeln(' %BuildDate% %BuildTime% %BuildDay% %BuildMonth% %BuildYear% %BuildHour% %BuildMinute% %BuildSecond%');
end;

procedure PrintVersion(FileName: TFileName; OutputFormat: string);
var
  FileVersion: TFileVersion;
  Output: string;

  procedure Replace(PlaceHolder: string; Value: string); overload;
  begin
    Output := StringReplace(Output, '%' + PlaceHolder + '%', Value, [rfReplaceAll]);
  end;

  procedure Replace(PlaceHolder: string; Value: Integer); overload;
  begin
    Replace(PlaceHolder, IntToStr(Value));
  end;

  procedure Replace(PlaceHolder: string; Value: Integer; PadLeftZeros: Integer); overload;
  begin
    Replace(PlaceHolder, IntToStr(Value).PadLeft(PadLeftZeros, '0'));
  end;

begin
  if OutputFormat = '' then
    OutputFormat :=
      'FileVersion: %FileMajor%.%FileMinor%.%FileRelease%.%FileBuild%' + sLineBreak +
      'ProductVersion: %ProductMajor%.%ProductMinor%.%ProductRelease%.%ProductBuild%' + sLineBreak +
      'BuildDate: %BuildDate% %BuildTime%';

  FileVersion := TFileVersion.Create(FileName);
  Output := OutputFormat;

  Replace('FileMajor', FileVersion.FileMajor);
  Replace('FileMinor', FileVersion.FileMinor);
  Replace('FileRelease', FileVersion.FileRelease);
  Replace('FileBuild', FileVersion.FileBuild);
  Replace('FileVersion32', FileVersion.FileVersion32);

  Replace('ProductMajor', FileVersion.ProductMajor);
  Replace('ProductMinor', FileVersion.ProductMinor);
  Replace('ProductRelease', FileVersion.ProductRelease);
  Replace('ProductBuild', FileVersion.ProductBuild);
  Replace('ProductVersion32', FileVersion.ProductVersion32);

  Replace('BuildDate', DateToStr(FileVersion.BuildDateTime));
  Replace('BuildTime', TimeToStr(FileVersion.BuildDateTime));
  Replace('BuildDay', DayOfTheMonth(FileVersion.BuildDateTime), 2);
  Replace('BuildMonth', MonthOfTheYear(FileVersion.BuildDateTime), 2);
  Replace('BuildYear', YearOf(FileVersion.BuildDateTime), 4);
  Replace('BuildHour', HourOf(FileVersion.BuildDateTime), 2);
  Replace('BuildMinute', MinuteOf(FileVersion.BuildDateTime), 2);
  Replace('BuildSecond', SecondOf(FileVersion.BuildDateTime), 2);

  Writeln(Output);
end;

begin
  try
    if ParamCount = 1 then
      PrintVersion(ParamStr(1), '')
    else if ParamCount = 2 then
      PrintVersion(ParamStr(1), ParamStr(2))
    else
      ShowUsage;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
