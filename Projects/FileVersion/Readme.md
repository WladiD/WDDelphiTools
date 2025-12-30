# FileVersion.exe
FileVersion.exe is a command line tool to fetch specific version informations of a EXE or DLL file and output it to the console. It is also able to format the version as you need.

It uses the unit [WDDT.FileVersion.pas](https://github.com/WladiD/WDDelphiTools/blob/master/WDDT.FileVersion.pas).

## Usage
```
FileVersion.exe file [format]

format can hold following placeholder:
 %FileMajor% %FileMinor% %FileRelease% %FileBuild% %FileVersion32%
 %ProductMajor% %ProductMinor% %ProductRelease% %ProductBuild% %ProductVersion32%
 %BuildDate% %BuildTime% %BuildDay% %BuildMonth% %BuildYear% %BuildHour% %BuildMinute% %BuildSecond%
```

## Examples
**Command**

    FileVersion "C:\Program Files (x86)\Google\Chrome\Application\chrome.exe"

Output

    FileVersion: 81.0.4044.138
    ProductVersion: 81.0.4044.138
    BuildDate: 01.05.2020 23:41:44

---

**Command**

    FileVersion "C:\Program Files (x86)\Google\Chrome\Application\chrome.exe" "V.%FileMajor% Release:%FileRelease%"

Output

    V.81 Release:4044

---

**Command**

    FileVersion "C:\Program Files (x86)\Google\Chrome\Application\chrome.exe" "%BuildYear%-%BuildMonth%"

Output

    2020-05
	
## Notes
* If you use format placeholder in a batch file, so you have to escape the %, so simply double it: %%
	
	

