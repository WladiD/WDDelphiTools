# DPT (Delphi Processing Tools)
This project was created for automated Delphi configuration from batch scripts. 

Currently it supports not all Delphi versions, but will be expanded on demand.

## Usage

### Overview
```
DPT.exe DelphiVersion Action [Parameters]
DPT.exe Help [Action]

Actions:
  Build <ProjectFile> [Platform] [Config] [ExtraArgs]
  BuildAndRun <ProjectFile> [Platform] [Config] [--OnlyIfChanged] [-- <Args>]
  DProjPrintConfigs <ProjectFile>
  DProjPrintCurConfig <ProjectFile>
  DProjPrintSearchPaths <ProjectFile> [Config] [Platform]
  HandleProtocol <dpt://URL>
  IsPackageRegistered <PackageFileName>
  OpenUnit <FullPathToUnit> [GoToLine <Line>] [GoToMemberImplementation <Name>]
  PrintPath <PathLiteral>
  RegisterPackage <PathToBPL>
  RemovePackage <PackageFileName>
  RemovePackagesBySourceDir <SourceDir>
  Start
  Stop

For more details use: DPT.exe Help Action
```

### Detailed Help
To see all available actions and their descriptions, run:

    DPT.exe Help

Output
```
Usage: DPT.exe Help <Action>

DelphiVersion:
  RECENT              Automatically selects the newest installed version
  D2007
  D10.1
  D10.3
  D11
  D12

Available Actions:
  Build <ProjectFile> [Platform] [Config] [ExtraArgs]
    Builds the specified project using MSBuild.
    Automatically sets up the environment variables (rsvars.bat) and passes the current Delphi version.   
    If <ProjectFile> is not a .dproj file, it is processed by the internal TmplCodeGen preprocessor first.
      - Supports embedded configs: (* Name-conf.json ... *)
      - Supports generation instructions: // TmplCodeGen Prefix
      - Supports include partials: // TmplCodeGen include_partials [Target]
    Defaults: Platform=Win32, Config=Debug
    Example: DPT RECENT Build MyProject.dproj Win64 Release "/t:Clean;Build"

  BuildAndRun <ProjectFile> [Platform] [Config] [--OnlyIfChanged] [-- <Args>]
    Builds and executes the project.
    Supports standard Build parameters and TmplCodeGen preprocessing (see Build action).
    --OnlyIfChanged: Skips build if executable is newer than source files.
    -- <Args>: Passes all subsequent arguments to the executable.
    Example: DPT RECENT BuildAndRun MyProject.dproj Win64 Release --OnlyIfChanged -- -run -debug

  DProjPrintConfigs <ProjectFile>
    Lists all build configurations defined in the specified .dproj file.
    Example: DPT D12 DProjPrintConfigs MyProject.dproj

  DProjPrintCurConfig <ProjectFile>
    Displays the default/active build configuration of the specified .dproj file.
    Example: DPT D12 DProjPrintCurConfig MyProject.dproj

  DProjPrintSearchPaths <ProjectFile> [Config] [Platform]
    Displays the effective unit search path for the project.
    Combines the project's specific search path (resolving variables) with the IDE's global library path.
    Defaults: Config=<ActiveConfig>, Platform=Win32.
    Example: DPT D12 DProjPrintSearchPaths MyProject.dproj Release Win64

  HandleProtocol <dpt://URL>
    Internal handler for "dpt://" URI schemes.
    Used to trigger actions like opening units from external applications (e.g., browsers or log viewers).
    Example: dpt://openunit/?file=C:\MyUnit.pas&line=50

  IsPackageRegistered <PackageFileName>
    Checks if a specific BPL package is currently registered in the IDE.
    Returns ExitCode 0 if registered, 1 if not.

  OpenUnit <FullPathToUnit> [GoToLine <Line>] [GoToMemberImplementation <Name>]
    Opens a source file in the Delphi IDE via the Slim Server plugin.
    Supports navigating to a specific line number or finding a member implementation (Class.Method).      
    Automatically starts the IDE if it is not running and waits for the plugin to become available.       

  PrintPath <PathLiteral>
    Outputs various IDE configuration paths to the console.
    Useful for build scripts to locate BDS, Bin, or default BPL/DCP output directories.
    Available literals:
    BDSPath, BDSBINPath,
    BPLOutputPath-Win32, BPLOutputPath-Win64,
    DCPOutputPath-Win32, DCPOutputPath-Win64

  RegisterPackage <PathToBPL>
    Registers a specific BPL file as a design-time package in the currently selected Delphi version.      

  RemovePackage <PackageFileName>
    Unregisters a design-time package by its file name (without path or extension).

  RemovePackagesBySourceDir <SourceDir>
    Scans the registry for design-time packages located inside the specified directory tree and unregisters them.

  Start
    Ensures the Delphi IDE is running.
    If not, it launches the process and waits for it to become responsive.
    Brings the IDE window to the front.

  Stop
    Forcefully terminates the running Delphi IDE process associated with the selected version.
    WARNING: Unsaved data will be lost.
```

## URL Protocol Registration
To use the `dpt://` URL protocol (e.g., to open units directly from a browser or other tools), you need to register it in Windows.

### _GenerateRegisterDptProtocol.bat
This script generates a Windows Registry file (`RegisterDptProtocol.reg`) that links the `dpt://` protocol to the current location of `DPT.exe`.

1. Run `_GenerateRegisterDptProtocol.bat`.
2. Execute the generated `RegisterDptProtocol.reg` file to update your registry.

Once registered, you can use links like:
`dpt://openunit/?file=C:\MyUnit.pas&line=42`

## Examples
### Print BDSBinPath
If you want simply to determine the bin path of a specific Delphi version, just try this:

    DPT.exe D10.1 PrintPath BDSBinPath

Output

    C:\Program Files (x86)\Embarcadero\Studio\18.0\bin

---

### Print BPLOutputPath

    DPT.exe D10.1 PrintPath BPLOutputPath-Win32

Output

    C:\Dev\Bpl

---

### Remove a single package

    DPT.exe D10.1 RemovePackage JclBaseExpert240

Output
```
Unregister design time package "JclBaseExpert240"...
C:\Dev\Bpl\JclBaseExpert240.bpl > deleted
```

---

### Remove packages
If you want to clean your registered design time packages by a specific folder, so call simply the following command:

    DPT.exe D10.1 RemovePackagesBySourceDir "C:\Dev\Bpl"
    
Output
```
Unregister design time packages contained in "C:\Dev\Bpl"...
C:\Dev\Bpl\BarcodeFastReport.bpl > deleted
C:\Dev\Bpl\BarcodeStudio.bpl > deleted
C:\Dev\Bpl\BarcodeStudioEditors.bpl > deleted
C:\Dev\Bpl\dclFrameViewerD10_1Berlin.bpl > deleted
C:\Dev\Bpl\dclfrx24.bpl > deleted
```
Note: The BPL files itself are not deleted as it looks like in output, but the registration in Delphi is.

---

### Register a design time package

    DPT.exe D10.1 RegisterPackage "C:\Dev\Bpl\JclBaseExpert240.bpl"

Output
```
Register design time package "C:\Dev\Bpl\JclBaseExpert240.bpl"...
Cleaning package cache for JclBaseExpert240.bpl
Cleaning ok
Registering package C:\Dev\Bpl\JclBaseExpert240.bpl
Registration ok
```

---

### Open a unit in IDE at specific line

    DPT.exe RECENT OpenUnit "C:\Projects\MyUnit.pas" GoToLine 42

---

### Open a unit and jump to member implementation

    DPT.exe RECENT OpenUnit "C:\WDC\WDDelphiTools\Projects\DPT\DPT.OpenUnitTask.pas" GoToMemberImplementation TDptOpenUnitTask.Execute

Output
```
Opening unit "C:\WDC\WDDelphiTools\Projects\DPT\DPT.OpenUnitTask.pas"...
Found member "TDptOpenUnitTask.Execute" at line 146.
Debug: Connection to 9012 failed: Zeitüberschreitung der Verbindung.
IDE Plugin not reachable via standard port. Checking IDE status...
Checking for running BDS instance...
Starting BDS: C:\Program Files (x86)\Embarcadero\Studio\23.0\bin\bds.exe
Waiting for BDS to become ready...
Waiting for main window to become visible and enabled...
.....[Handle:394696 Vis:False En:True Title:"Delphi 12"].....[Handle:394696 Vis:False En:True Title:"DPT - Delphi 12 - ProjectGroup1.groupproj"]. Window 460232 is ready: "DPT - Delphi 12 - ProjectGroup1.groupproj"

IDE is ready. Scanning for listening Slim ports (9000-9100) on PID 25656...
 Found candidate port 9012. Trying to connect...
Successfully opened unit via IDE Plugin.
```

---

### Analyze Project (.dproj)

**List Configs:**

    DPT.exe D12 DProjPrintConfigs MyProject.dproj

Output
```
Debug
Release
```

**Show Active Config:**

    DPT.exe D12 DProjPrintCurConfig MyProject.dproj

Output
```
Debug
```

**Show Effective Search Paths:**

    DPT.exe D12 DProjPrintSearchPaths MyProject.dproj Release Win64

Output
```
C:\Program Files (x86)\Embarcadero\Studio\23.0\lib\win64\release
C:\MyProject\Source
C:\MyProject\Common
```

---

### Build a project

    DPT.exe D12 Build TmplCodeGen.dproj

Output
```
Setting up Delphi environment from: C:\Program Files (x86)\Embarcadero\Studio\23.0\bin\rsvars.bat
PRODUCTVERSION: 23.0
Building TmplCodeGen.dproj...
Microsoft (R)-Buildmodul, Version 4.8.9032.0
[Microsoft .NET Framework, Version 4.0.30319.42000]
Copyright (C) Microsoft Corporation. Alle Rechte vorbehalten.
...
Build succeeded.
```

---

### Build and Run a project

    DPT.exe RECENT BuildAndRun TmplCodeGen.dproj Win32 Debug --OnlyIfChanged

Output (First run)
```
Setting up Delphi environment from: C:\Program Files (x86)\Embarcadero\Studio\23.0\bin\rsvars.bat
PRODUCTVERSION: 23.0
Building C:\...\TmplCodeGen.dproj...
Microsoft (R)-Buildmodul, Version 4.8.9032.0
...
Build succeeded.
Running C:\...\TmplCodeGen.exe ...
--------------------------------------------------
TmplCodeGen.exe prefix
TmplCodeGen.exe include_partials target_file
```

Output (Subsequent run with `--OnlyIfChanged`)
```
Executable is up to date. Skipping build.
Running C:\...\TmplCodeGen.exe ...
--------------------------------------------------
TmplCodeGen.exe prefix
TmplCodeGen.exe include_partials target_file
```
