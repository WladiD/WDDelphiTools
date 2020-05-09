# Usage of FileVersion.exe
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

**Command**

    FileVersion "C:\Program Files (x86)\Google\Chrome\Application\chrome.exe" "V.%FileMajor% Release:%FileRelease%"
*Note:* If you use format placeholder in a batch file, so you have to escape the % with %%

Output

    V.81 Release:4044
    
