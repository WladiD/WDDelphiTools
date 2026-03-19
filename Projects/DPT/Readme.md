# DPT (Delphi Processing Tools)

DPT is the "Swiss Army Knife" for Delphi development, created for automated Delphi configuration, CI/CD pipelines, and AI-assisted workflows from batch scripts. 

Currently, it supports selected Delphi versions (automatically prioritizing the `LATEST` installed version) and gets expanded on demand.

## 🚀 Features & Usage

Run `DPT.exe Help <Action>` for detailed information on any specific command. Below is an overview of the core capabilities grouped by use cases.

### 🤖 AI & Debugging

DPT provides special support for AI agents through rule-based workflows and the Model Context Protocol (MCP).

*   **`McpDebugger`**: Starts a standalone MCP server for debugging Delphi applications. It runs continuously in the background and provides tools for AI agents (like Gemini, Claude) to:
    *   Manage debug sessions (`start_debug_session`, `stop_debug_session`, `terminate_debug_session`)
    *   Set/remove hardware breakpoints (`set_breakpoint`, `remove_breakpoint`, `list_breakpoints`)
    *   Control asynchronous execution (`continue`, `step_into`, `step_over`, `wait_until_paused`)
    *   Inspect state and memory (`get_state`, `get_stack_trace`, `get_registers`, `get_stack_slots`, `get_stack_memory`, `read_memory`, `read_global_variable`, `get_proc_asm`)
*   **`AiSession`**: Manages an AI session for the current process hierarchy. Uses an internal workflow engine to provide instructions and track state (e.g., Lint results). See [AiWorkflow.md](AiWorkflow.md) for details.

### 🏗️ Build-Management & CI/CD

Commands to automate compilation and manage portable build environments.

*   **`Build`**: Builds a project using MSBuild. Automatically sets up `rsvars.bat` environments. Supports embedded configs and instructions via the internal `TmplCodeGen` preprocessor.
    ```cmd
    DPT.exe LATEST Build TmplCodeGen.dproj Win32 Debug
    ```
*   **`BuildAndRun`**: Builds and subsequently executes a project, skipping build if executable is up-to-date (`--OnlyIfChanged`).
    ```cmd
    DPT.exe LATEST BuildAndRun MyProject.dproj Win64 Release --OnlyIfChanged -- -run -debug
    ```
*   **`ExportBuildEnvironment` / `ImportBuildEnvironment`**: Exports a minimal Delphi build environment (binaries, registry settings, init scripts) for CI/CD pipelines on clean Windows machines (valid license still required).
    ```cmd
    DPT.exe D12 ExportBuildEnvironment C:\VM-Share\Delphi12BuildEnvironment\
    ```

### 🔍 Project- & Code-Analysis

Automate `.dproj` extraction and enforce code style rules.

*   **`.dproj` Analysis**: Read active configurations, output paths, and search paths.
    *   `DPT.exe D12 DProjPrintConfigs MyProject.dproj`
    *   `DPT.exe D12 DProjPrintCurConfig MyProject.dproj`
    *   `DPT.exe D12 DProjPrintOutputFile MyProject.dproj Release Win64`
    *   `DPT.exe D12 DProjPrintSearchPaths MyProject.dproj Release Win64`
*   **`Lint`**: Analyzes Delphi units for style violations using an internal Slim/FitNesse engine.
    ```cmd
    DPT.exe LATEST Lint --verbose Lint\TaifunUnitStyle.pas Unit1.pas Unit2.pas
    ```
*   **`LintSetup`**: Split or join style files into templates and descriptions for easier editing.

### 🔌 IDE Automation & Packages

Control the Delphi IDE, register packages natively from the command line, and open files via custom protocols.

*   **`OpenUnit`**: Opens a Delphi source file in the IDE at a specific line or member implementation. Starts the IDE if necessary.
    ```cmd
    DPT.exe LATEST OpenUnit "C:\Projects\MyUnit.pas" GoToLine 42
    DPT.exe LATEST OpenUnit "C:\Projects\MyUnit.pas" GoToMemberImplementation TMyClass.Execute
    ```
*   **`RegisterPackage` / `RemovePackage`**: Manage design-time packages (`.bpl`).
    ```cmd
    DPT.exe D12 RegisterPackage "C:\Dev\Bpl\JclBaseExpert240.bpl"
    DPT.exe D12 RemovePackage JclBaseExpert240
    DPT.exe D12 RemovePackagesBySourceDir "C:\Dev\Bpl"
    ```
*   **`PrintPath` / `Start` / `Stop`**: Output IDE internal paths (`BDSBinPath`), manually start or violently terminate the IDE process.

## 🔗 URL Protocol Registration (`dpt://`)

To use the `dpt://` URL protocol (e.g., to open units directly from a browser or log viewers), you need to register it in Windows.

1. Run `_GenerateRegisterDptProtocol.bat`.
2. Execute the generated `RegisterDptProtocol.reg` file to update your registry.

Once registered, you can use links like:
`dpt://openunit/?file=C:\MyUnit.pas&line=42`
