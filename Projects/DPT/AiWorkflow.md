# DPT AI-Workflows (.DptAiWorkflow)

DPT supports an "AI-Mode" specifically designed for collaboration with AI agents (like the Gemini CLI). A file named `.DptAiWorkflow` in the project directory defines rules that control the workflow, display instructions, or handle errors via guards.

## How it works
On startup, DPT recursively searches upwards for a file named `.DptAiWorkflow`. If DPT is executed within an AI session (detected via the host PID), the internal `TDptWorkflowEngine` evaluates this file in two phases:

1.  **BeforeDptGuard:** Evaluated *before* the main task is executed. Used for validation, ensuring requirements, and preventing invalid states.
2.  **AfterDptGuard:** Evaluated *after* the task (or after a `BeforeDptGuard` violation). Used for reporting, custom error messages, or post-task cleanup instructions.

## File Format
The file consists of guard blocks:

```text
BeforeDptGuard: <expression>
{
  Instructions for the AI go here...
  You can use backticks to evaluate expressions like `GetExitCode()`.
    
  BeforeDptGuard: RequestDptExitWithCode(12)  # Request exit with specific code
}

AfterDptGuard: GetExitCode() > 0
{
  An error occurred!
  ExitCode: `GetExitCode()`
}
```

- **Comments:** Lines starting with `#` are ignored.
- **Backtick Evaluation:** Text inside `` ` `` is evaluated as an expression by the parser. If it's not a valid expression, it remains unchanged.
- **Escaping Backticks:** Use double backticks (`` ` `` `` ` ``) to output a literal backtick (`` ` ``).
- **Multiple Blocks:** Any number of blocks can be defined. Met conditions are processed sequentially.
- **Nesting:** Guards can be nested within blocks to create complex logical flows.

## Editor Support
For easier editing of `.DptAiWorkflow` files, a User Defined Language (UDL) file for **Notepad++** is available:
- File: `Projects/DPT/Extras/userDefineLang_DptAiWorkflow.xml`
- Installation: In Notepad++, go to `Language` -> `User Defined Language` -> `Define your language...`, then click `Import...` and select the file.

## Available Functions (Expression Parser)

The guards use an internal expression parser with access to the current DPT state:

| Function | Description |
| :--- | :--- |
| `IsCurrentAction("Name")` | Checks the current DPT action (e.g., "Build", "Lint", "BuildAndRun"). |
| `IsCurrentBuildProjectFile("File.dproj")` | Checks if the current project file name matches the provided name (case-insensitive). |
| `GetCurrentProjectFiles()` | Returns an array of all source files for the project currently being processed by `Build` or `BuildAndRun`. |
| `RequestDptExit()` | Signals the engine to exit the process after the guard phase. |
| `RequestDptExitWithCode(Code)` | Signals an exit and sets the process exit code. |
| `GetExitCode()` | Returns the current exit code (0 if successful or the code set by a guard). |
| `FixLineEndingsWindowsInGitModifiedFiles()` | Converts line endings of all uncommitted files in Git to CRLF (Windows), respecting active `IgnorePattern`. Returns `True` if any file was changed. |
| `FixUtf8BomInGitModifiedFiles()` | Ensures all uncommitted files in Git are encoded in UTF-8 with BOM, respecting active `IgnorePattern`. Returns `True` if any file was changed. |
| `GetLastFixedFiles()` | Returns a newline-separated list of files that were modified by the last executed Fix function. |
| `FormatGitModifiedFiles("Script.pas")` | Formats all uncommitted files using the provided DWS script, respecting active `IgnorePattern` and `IncludePattern`. Returns `True` if any file was changed. |
| `GetLastFormattedFiles()` | Returns a newline-separated list of files that were modified by `FormatGitModifiedFiles`. |
| `IncludePattern("P1", "P2", ...)` | Adds glob patterns (e.g. `*.txt`, `*.md`) to the include list used by Fix and Format functions. If the include list is not empty, only files matching at least one pattern are processed. Always returns `True` for chaining. |
| `ClearIncludePatterns()` | Clears the current list of include patterns. Always returns `True` for chaining. |
| `IgnorePattern("P1", "P2", ...)` | Adds glob patterns (e.g. `*\MISC\*`, `*.dfm`) to the ignore list used by Fix and Format functions. Files matching these patterns are excluded, even if they match an `IncludePattern`. Always returns `True` for chaining. |
| `ClearIgnorePatterns()` | Clears the current list of ignore patterns. Always returns `True` for chaining. |

## Example Workflow

A scenario ensuring code quality before a build or running fix scripts:

```text
# Abort if the user tries to run a build without specifying a project
BeforeDptGuard: IsCurrentAction("Build") and IsCurrentBuildProjectFile("")
{
  - Please specify a .dproj file to build!
    
  BeforeDptGuard: RequestDptExitWithCode(1)
}

AfterDptGuard: GetExitCode() > 0
{
  Process stopped with ExitCode `GetExitCode()`.
}

# Auto-Fix Line Endings and BOM
BeforeDptGuard: IsCurrentAction("Build")
{
  BeforeDptGuard: FixLineEndingsWindowsInGitModifiedFiles()
  {
    - Fixed CRLF line endings in:
      - `GetLastFixedFiles()`
  }
  
  BeforeDptGuard: FixUtf8BomInGitModifiedFiles()
  {
    - Fixed missing UTF-8 BOM in:
      - `GetLastFixedFiles()`
  }
}
```

## Session Management
To persist state (e.g., lint results) across multiple DPT calls, a session file (`.DptAiWorkflow.Session<PID>.json`) is used. This is controlled via the `AiSession` action:

1. `DPT LATEST AiSession Start`: Initializes the session and sets the baseline time for change detection.
2. `DPT LATEST AiSession Reset`: Reset session without changing the baseline point (Useful if testing goes wrong)
3. `DPT LATEST AiSession Status`: Show current session details
4. `DPT LATEST AiSession Stop`: Ends the session and deletes temporary data.
