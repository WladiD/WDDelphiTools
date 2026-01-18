# WDDelphiTools

**WDDelphiTools** is a collection of useful libraries (units), command-line tools, and examples for Delphi development. This repository provides utilities to boost productivity, optimize compile times, and extend the Delphi RTL/VCL.

## Tools (Projects)

The `Projects/` folder contains standalone tools:

*   **[DcuCompileTimes](Projects/DcuCompileTimes/Readme.md)**: An analysis tool that identifies compilation bottlenecks based on `.dcu` file timestamps.
*   **[DPT (Delphi Processing Tools)](Projects/DPT/Readme.md)**: A CLI tool for managing Delphi installations and packages (registering/unregistering BPLs, opening units in the IDE).
*   **[FileVersion](Projects/FileVersion/Readme.md)**: A command-line tool for outputting version information of binary files (based on `WDDT.FileVersion.pas`).
*   **[StaticGenerics](Projects/StaticGenerics/README.md) & [TmplCodeGen](Projects/TmplCodeGen/README.md)**: A system for generating static, typed lists and dictionaries at compile time to improve compilation speed when using generics extensively.

## Libraries (Core Units)

The main library consists of a set of `WDDT.*.pas` units in the root directory, which can be directly included in your projects:

*   **`WDDT.DelayedMethod.pas`**: Enables delayed method execution, cleanly synchronized with the main thread (via Timer). Ideal for decoupling method calls or "debouncing" multiple calls.
*   **`WDDT.FileVersion.pas`**: An efficient helper for reading file and product versions from Executables or DLLs.
*   **`WDDT.StringTools.pas`**: High-performance routines for string manipulation, including optimized `Empty`/`Defined` checks.
*   **`WDDT.RTTI.pas`**: Helper functions for Runtime Type Information (RTTI), e.g., for copying object properties (`CopyObject`).
*   **`WDDT.Vcl.Controls.pas`**: A collection of helper functions for working with VCL controls (e.g., recursive search for parents/children, scrollbar checks, etc.).
*   **`WDDT.Vcl.RTTI.pas`**: Specific RTTI functions for VCL components, such as `DeepCloneComponent`.
*   **`WDDT.MobileTools.pas`**: Tools for mobile development (e.g., `KeepScreenOn` for Android).

## Examples

The `Examples/` folder contains demonstrations and benchmarks:

*   **[GenericsStudy](Examples/GenericsStudy/README.md)**: A comprehensive benchmark comparing different approaches to generics in Delphi (Delphi Standard, Spring4D, mORMot, Static Code Gen) focusing on compile time and binary size.
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
