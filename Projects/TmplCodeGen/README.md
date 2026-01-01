# TmplCodeGen

**TmplCodeGen** is a specialized code generation tool for Delphi, powered by the [mORMot](https://github.com/synopse/mORMot2) Mustache template engine. It allows for the generation of complex, repetitive Delphi code (like static generic collections) based on JSON configuration and Pascal-based templates.

## 🚀 Key Features

*   **Mustache Templating:** Uses the powerful Mustache logic-less template syntax to generate code.
*   **JSON Configuration:** Controlled via JSON files defining types and parameters.
*   **Partial Injection:** Can split generated code into "partials" and inject them into specific regions of existing source files, preserving the rest of the file.
*   **Smart Post-Processing:** Includes "PostFix" commands to handle tricky syntax issues like trailing commas or semicolons in generated lists.
*   **GUID Preservation:** When regenerating interfaces, it attempts to preserve existing GUIDs to maintain binary compatibility.

## 🛠️ Build

The project includes a helper script to build and run the tool automatically.

**Prerequisites:**
*   Delphi 12 (Athens) installed (Default path: `C:\Program Files (x86)\Embarcadero\Studio\23.0`).
*   mORMot 2 library (included in `Lib/mORMot`).

**Build & Run:**
Simply execute the batch file in the project root:

```cmd
_TmplCodeGen.BuildAndRun.bat <Arguments>
```

This script will:
1.  Check if `TmplCodeGen.exe` exists or is older than 5 minutes.
2.  Rebuild it if necessary using `Source/_TmplCodeGen.Build.bat`.
3.  Run the executable with the provided arguments.

## 📖 Usage

### 1. Generate Code

To generate code from a template and a JSON config:

```cmd
TmplCodeGen.exe <Prefix>
```

*   **`<Prefix>`**: The base name for your configuration files.
    *   The tool looks for `<Prefix>-conf.json`.
    *   The JSON must specify a `"Template"` key (e.g., `MyTemplate.TMPL.pas`).
    *   The tool generates `<Prefix>.pas`.
    *   It also generates `<Prefix>-<PartialName>.part.pas` for any defined partials.

### 2. Inject Partials

To inject generated partials into a target source file:

```cmd
TmplCodeGen.exe include_partials <TargetFile>
```

*   **`<TargetFile>`**: The Delphi unit where the code should be injected.
*   The target file must contain `{$REGION 'INCLUDE-PARTIAL / <PartialFileName>'}` blocks.
*   The tool replaces the content within these regions with the content of the corresponding `.part.pas` files.

## 🧩 Template Features

### PostFix Commands
You can add special comments in your template to trigger post-processing logic:

*   `// PostFixParamsDefine`: Removes trailing semicolons in parameter lists.
    *   `procedure Foo(A: Integer; B: String;); // PostFixParamsDefine` -> `procedure Foo(A: Integer; B: String);`
*   `// PostFixParamsCall`: Removes trailing commas in argument lists.
    *   `CallFoo(1, 'Text',); // PostFixParamsCall` -> `CallFoo(1, 'Text');`

### Partials Definition
In your template, define regions that should be extracted as partials:

```delphi
{$REGION 'DEFINE-PARTIAL / MyPartialName'}
  // ... generated code ...
{$ENDREGION 'DEFINE-PARTIAL / MyPartialName'}
```

## 📂 Project Structure

*   **`Source/`**: Delphi source code for the tool.
*   **`Test/`**: Unit tests to ensure generation logic is correct.
*   **`_TmplCodeGen.BuildAndRun.bat`**: Main entry point for building and running.

## 📝 License

Licensed under the MIT License.
