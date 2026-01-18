# DptIdeExpert

A Delphi IDE Expert (Wizard) that embeds a **Slim Server** to allow remote control and automation of the IDE via the [Slim Protocol](http://fitnesse.org/FitNesse.UserGuide.WritingAcceptanceTests.SliM) (used by FitNesse).

This expert is currently used by the **DPT (Delphi Processing Tools)** CLI to reliably open units and navigate to a specific line or method from the command line or external scripts.

## Features

- **Embedded Slim Server:** Starts a TCP server within the IDE process upon loading.
- **Port Calculation:** Automatically determines the listening port based on the running Delphi version:
  - **Delphi 12 (Athens):** Port 9012
  - **Delphi 11 (Alexandria):** Port 9011
- **IDE Integration:** Logs server status (start, stop, errors) to the IDE's "Messages" view under the "DPT" group.
- **Automation Fixtures:** Provides Slim fixtures to control the IDE.
  - `TDptIdeOpenUnitFixture`: Opens a unit file and jumps to a specific line.

## Installation

1.  **Build** the package using the provided script:
    ```cmd
    _DptIdeExpert.Build.bat
    ```
2.  **Register** the BPL in the IDE:
    ```cmd
    _DptIdeExpert.RegisterBpl.bat D12
    ```
    *(Replace `D12` with your target Delphi version, e.g., `D11`)*

    Alternatively, you can manually install the generated `.bpl` via the IDE menu:
    `Component` -> `Install Packages...` -> `Add...`

## Usage

Once installed, the Slim Server runs automatically in the background. You can interact with it using any Slim client, but the most common use case is via **DPT.exe**:

```cmd
DPT.exe RECENT OpenUnit "C:\MyProject\MyUnit.pas" GoToLine 42
```

This command connects to the `DptIdeExpert` running in the most recent Delphi instance and instructs it to open `MyUnit.pas` and place the cursor on line 42.

## Project Structure

- **`DptIdeExpert.Wizard.pas`**: The main entry point. Implements `IOTAWizard` to initialize the Slim Server and handles IDE logging.
- **`DptIdeExpert.Fixtures.pas`**: Contains the Slim fixtures (e.g., `TDptIdeOpenUnitFixture`) that expose IDE functionality (via `ToolsAPI`) to the Slim protocol.
- **`_DptIdeExpert.Build.bat`**: Script to compile the project.
- **`_DptIdeExpert.RegisterBpl.bat`**: Helper script to register the compiled BPL using `DPT.exe`.
