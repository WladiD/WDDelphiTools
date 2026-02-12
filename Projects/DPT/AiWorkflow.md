# DPT AI-Workflows (.DptAiWorkflow)

DPT supports an "AI-Mode" specifically designed for collaboration with AI agents (like the Gemini CLI). A file named `.DptAiWorkflow` in the project directory can be used to define rules that control the workflow, display instructions, or abort the process in case of rule violations.

## How it works
On startup, DPT recursively searches upwards for a file named `.DptAiWorkflow`. If DPT is executed within an AI session (detected via the host PID), the internal `TDptWorkflowEngine` evaluates this file.

## File Format
The file consists of condition blocks:

```text
DptCondition: <expression>
{
  Instructions for the AI go here...
  These will be printed to the console if the condition is met.
    
  DptAction: ExitDptProcess()  # Optional: Aborts DPT with an error code
}
```

- **Comments:** Lines starting with `#` are ignored.
- **Multiple Blocks:** Any number of blocks can be defined. All met conditions are processed sequentially.

## Available Functions (Expression Parser)

The conditions use an internal expression parser with access to the current DPT state:

| Function | Description |
| :--- | :--- |
| `AiSessionStarted()` | Returns `True` if an active AI session exists. |
| `IsCurrentAction("Name")` | Checks the current DPT action (e.g., "Build", "Lint"). |
| `IsCurrentAiSessionAction("Sub")` | Checks the sub-action of `AiSession` (e.g., "Start", "Status"). |
| `GetCurrentLintTargetFile()` | Returns the path of the file currently being processed by `Lint`. |
| `GetCurrentProjectFiles()` | Returns an array of all source files for the project currently being processed by `Build`. |
| `IsFileRegisteredInAiSession(File)` | Checks if a file has already been registered in the session. |
| `HasValidLintResult(Files)` | Checks if there are current and successful lint results for the provided files. |

## Example Workflow: Mandatory Lint before Build

A typical scenario is ensuring that no unverified files are built:

```text
DptCondition: IsCurrentAction("Build") and 
              not HasValidLintResult(GetCurrentProjectFiles())
{
  - A valid lint result must exist for all changed files before building!
  - Run first: `DPT LATEST Lint TaifunUnitStyle.pas <TargetFile>`
    
  DptAction: ExitDptProcess()
}
```

## Session Management
To persist state (e.g., lint results) across multiple DPT calls, a session file (`.DptAiWorkflow.Session<PID>.json`) is used. This is controlled via the `AiSession` action:

1. `DPT LATEST AiSession Start`: Initializes the session.
2. `DPT LATEST AiSession RegisterFiles <Files>`: Registers files for tracking.
3. `DPT LATEST AiSession Status`: Displays the current status.
4. `DPT LATEST AiSession Stop`: Ends the session and deletes temporary data.
