# DPT.exe
This project was created for automated Delphi configuration from batch scripts. 

Currently it supports not all Delphi versions, but will be expanded on demand.

## Usage
```
DPT.exe DelphiVersion Mode [OtherModeSpecificParameters]

  DelphiVersion
    D2007
    D10.1
    D10.3

  Mode
    RemovePackagesBySourceDir SourceDir
      Removes the registration of design time packages for the defined
      Delphi-IDE which are located in SourceDir

    RemovePackage PackageFileName
      Removes the design time registration of the specified package by their
      name (without file extension) only.

    RegisterPackage PathToBPL
      Register a package specified in PathToBPL as design time package

    PrintPath (BDSPath|BDSBINPath|BPLOutputPath-Win32|BPLOutputPath-Win64|DCPOutputPath-Win32|DCPOutputPath-Win64)
      Prints the path
```

## Examples
### Print BDSBinPath
If you want simply to determine the bin path of a specific Delphi version, just try this:

    DPT D10.1 PrintPath BDSBinPath

Output

    C:\Program Files (x86)\Embarcadero\Studio\18.0\bin

---

### Print BPLOutputPath

    dpt D10.1 PrintPath BPLOutputPath-Win32

Output

    C:\Dev\Bpl

---

### Remove a single package

    DPT D10.1 RemovePackage JclBaseExpert240

Output
```
Unregister design time package "JclBaseExpert240"...
C:\Dev\Bpl\JclBaseExpert240.bpl > deleted
```

---

### Remove packages
If you want to clean your registered design time packages by a specific folder, so call simply the following command:

    DPT D10.1 RemovePackagesBySourceDir "C:\Dev\Bpl"
    
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

    DPT D10.1 RegisterPackage "C:\Dev\Bpl\JclBaseExpert240.bpl"

Output
```
Register design time package "C:\Dev\Bpl\JclBaseExpert240.bpl"...
Cleaning package cache for JclBaseExpert240.bpl
Cleaning ok
Registering package C:\Dev\Bpl\JclBaseExpert240.bpl
Registration ok
```
