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

## Available Functions (Expression Parser)

The guards use an internal expression parser with access to the current DPT state:

| Function | Description |
| :--- | :--- |
| `AiSessionStarted()` | Returns `True` if an active AI session exists. |
| `IsCurrentAction("Name")` | Checks the current DPT action (e.g., "Build", "Lint"). |
| `IsCurrentAiSessionAction("Sub")` | Checks the sub-action of `AiSession` (e.g., "Start", "Status"). |
| `GetCurrentLintTargetFile()` | Returns the path of the file currently being processed by `Lint`. |
| `GetCurrentProjectFiles()` | Returns an array of all source files for the project currently being processed by `Build`. |
| `IsFileRegisteredInAiSession(File)` | Checks if a file has already been registered in the session. |
| `HasValidLintResult(Files)` | Checks if successful lint results exist for the provided files. |
| `RequestDptExit()` | Signals the engine to exit the process after the guard phase. |
| `RequestDptExitWithCode(Code)` | Signals an exit and sets the process exit code. |
| `GetExitCode()` | Returns the current exit code (0 if successful or the code set by a guard). |

## Example Workflow: Mandatory Lint before Build

A typical scenario is ensuring that no unverified files are built:

```text
BeforeDptGuard: IsCurrentAction("Build") and 
                not HasValidLintResult(GetCurrentProjectFiles())
{
  - A valid lint result must exist for all changed files before building!
  - Run first: `DPT LATEST Lint TaifunUnitStyle.pas <TargetFile>`
    
  BeforeDptGuard: RequestDptExitWithCode(13)
}

AfterDptGuard: GetExitCode() > 0
{
  Process stopped with ExitCode `GetExitCode()`.
}
```

## Session Management
To persist state (e.g., lint results) across multiple DPT calls, a session file (`.DptAiWorkflow.Session<PID>.json`) is used. This is controlled via the `AiSession` action:

1. `DPT LATEST AiSession Start`: Initializes the session.
2. `DPT LATEST AiSession RegisterFiles <Files>`: Registers files for tracking.
3. `DPT LATEST AiSession Status`: Displays the current status.
4. `DPT LATEST AiSession Stop`: Ends the session and deletes temporary data.

