# DcuCompileTimes

`DcuCompileTimes` is a simple command-line tool for Delphi developers to analyze build output and identify compilation bottlenecks.

It scans all `.dcu` files in the current directory, analyzes their timestamps, and calculates the time taken to compile each unit.

## How It Works

The tool's logic relies on the file modification timestamps created by the Delphi compiler during a build.

1.  **Scan:** It scans the *current directory* for all `*.dcu` files.
2.  **Timestamp:** It fetches the precise modification timestamp for every file.
3.  **Sort (by Time):** It sorts this list of files by their timestamp, from newest to oldest.
4.  **Calculate Deltas:** It then iterates through this sorted list and calculates the time *difference* (delta) between each file and the file immediately following it (the one compiled just before it).
5.  **Identify Duration:** This time-delta is considered the "compilation time" for that specific unit.
6.  **Calculate Stats:** It sums all deltas to get the **Total Time** and also calculates the **Median** compilation time.
7.  **Sort (by Duration):** Finally, it re-sorts the list by the calculated compilation time (longest first).
8.  **Report:** It prints a report to the console, showing the total and median times, followed by the list of units and their individual compilation times.

## Why This Method is Accurate

This tool's assumption is correct because the Delphi compiler (`dcc32.exe`, `dcc64.exe`) is **single-threaded**.

Unlike some other compilers (like C++), the Delphi compiler always processes the units within a single project **serially** (one after another). Because of this, the file modification timestamps are created in a strict sequence, making the time delta between them a valid measure of compilation duration.

## Usage

1.  To get accurate results, first perform a **full, clean "Build"** (not just "Compile") of your Delphi project.
2.  Open a command prompt (CMD, PowerShell, etc.).
3.  Navigate (`cd`) to the directory containing your newly generated `.dcu` files (e.g., `Win32\Debug` or `dcu\Win32\Release`).
4.  Run the executable (`DcuCompileTimes.exe`) in that directory.

## Example Output

```bash
Total time: 128,1114 ms
Median time: 5,0939 ms
---------------------------------------------------
Files, sorted by generation time (ms), factor to median (x):
---------------------------------------------------
WDDT.DelayedMethod.dcu                   (48,5557 ms) (x9,53)
Test.SlimFixture.dcu                     (30,9445 ms) (x6,07)
Slim.Fixture.dcu                         (11,9477 ms) (x2,35)
Test.SlimExec.dcu                        (7,0151 ms) (x1,38)
Slim.Exec.dcu                            (6,8941 ms) (x1,35)
Slim.Common.dcu                          (5,0980 ms) (x1,00)
Slim.List.dcu                            (5,0898 ms) (x1,00)
Slim.Symbol.dcu                          (4,0223 ms) (x0,79)
TestInsight.DUnitX.dcu                   (4,0053 ms) (x0,79)
Test.SlimSymbol.dcu                      (2,5042 ms) (x0,49)
Test.SlimList.dcu                        (2,0347 ms) (x0,40)
---------------------------------------------------
```