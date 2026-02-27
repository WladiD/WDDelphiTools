# WDDelphiTools

**WDDelphiTools** is a collection of useful libraries (units), command-line tools, and examples for Delphi development. This repository provides utilities to boost productivity, optimize compile times, and create automated build/AI pipelines for the Delphi ecosystem.

## 🛠️ Tools (Projects)

*   **[DPT (Delphi Processing Tools)](Projects/DPT/Readme.md)**: The "Swiss Army Knife" for Delphi. A comprehensive CLI tool providing:
    *   **AI & Debugging:** A standalone MCP (Model Context Protocol) Server for AI agents and a Workflow Engine for AI-assisted code analysis (`AiSession`, `McpDebugger`).
    *   **Build-Management & CI/CD:** Command-Line Building with preprocessor support and the ability to export portable build environments.
    *   **Project & Code Analysis:** Automated extracting of `.dproj` configurations and FitNesse/Slim-based linting.
    *   **IDE Automation:** Registering/unregistering design-time packages (BPLs) and opening units directly in the IDE via the `dpt://` protocol.
*   **[DcuCompileTimes](Projects/DcuCompileTimes/Readme.md)**: An analysis tool that identifies compilation bottlenecks based on `.dcu` file timestamps.
*   **[FileVersion](Projects/FileVersion/Readme.md)**: A command-line tool for outputting version information of binary files.
*   **[StaticGenerics](Projects/StaticGenerics/README.md) & [TmplCodeGen](Projects/TmplCodeGen/README.md)**: A system for generating static, typed lists and dictionaries at compile time to improve compilation speed when using generics extensively.

## 📚 Libraries (Core Units)

The main library consists of a set of `WDDT.*.pas` units in the root directory, which can be directly included in your projects:

*   **`WDDT.DelayedMethod.pas`**: Enables delayed method execution, cleanly synchronized with the main thread (via Timer). Ideal for decoupling method calls or "debouncing" multiple calls.
*   **`WDDT.FileVersion.pas`**: An efficient helper for reading file and product versions from Executables or DLLs.
*   **`WDDT.StringTools.pas`**: High-performance routines for string manipulation.
*   **`WDDT.RTTI.pas`**: Helper functions for Runtime Type Information (RTTI), e.g., for copying object properties (`CopyObject`).
*   **`WDDT.Vcl.Controls.pas`**: A collection of helper functions for working with VCL controls.
*   **`WDDT.Vcl.RTTI.pas`**: Specific RTTI functions for VCL components, such as `DeepCloneComponent`.
*   **`WDDT.MobileTools.pas`**: Tools for mobile development (e.g., `KeepScreenOn` for Android).

## 💡 Examples

The `Examples/` folder contains demonstrations and benchmarks:

*   **[GenericsStudy](Examples/GenericsStudy/README.md)**: A comprehensive benchmark comparing different approaches to generics in Delphi (focusing on compile time and binary size).
*   **VclControls**: Examples for using the `WDDT.Vcl.*` helpers.

## Installation & Usage

1.  Clone the repository:
    ```bash
    git clone https://github.com/WladiD/WDDelphiTools.git
    ```
2.  Add the path to the `WDDT.*.pas` files to your Delphi library path or search path to use the core units.
3.  The tools in the `Projects` folder can be compiled directly with Delphi (usually `.bat` scripts are available for building).

## License

This project is licensed under the [MIT License](LICENSE).
