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
2.  Open a command prompt (CMD, PowerShell).
3.  Navigate (`cd`) to the directory containing your newly generated `.dcu` files (e.g., `Win32\Debug`).
4.  Run the executable (`DcuCompileTimes.exe`) in that directory.

## Command Line Arguments

```
DcuCompileTimes.exe [Options] [FilterMask]

Analyzes Delphi .dcu compilation times in the current directory.

Arguments:
  [FilterMask]  Optional. A file mask (e.g., "Base.*", "Base.Db.*") to filter
                the displayed files and calculate a "Mask time" summary.

Options:
  --help, -h, ? Displays this help message.
  --top-ns=n    Shows the top n namespaces by compile time.
                n can be a number, "all", "0", or "off". Default is 10.
```

## Example Output

```console
------------------------------------------------------------
Files, sorted by generation time (ms), factor to median (x):
------------------------------------------------------------
WDDT.DelayedMethod.dcu                   (47,4691 ms) (x8,02)
Slim.Fixture.dcu                         (16,0846 ms) (x2,72)
Test.SlimExec.dcu                        (7,0604 ms) (x1,19)
Test.SlimFixture.dcu                     (6,5990 ms) (x1,11)
Slim.Exec.dcu                            (6,3514 ms) (x1,07)
Slim.Common.dcu                          (5,9580 ms) (x1,01)
Slim.List.dcu                            (5,8812 ms) (x0,99)
TestInsight.DUnitX.dcu                   (5,7495 ms) (x0,97)
Slim.Symbol.dcu                          (5,0161 ms) (x0,85)
Test.SlimSymbol.dcu                      (2,9430 ms) (x0,50)
Test.SlimList.dcu                        (2,5139 ms) (x0,42)
TestInsight.Client.dcu <- build start 09.11.2025 19:09:21
------------------------------------------------------------
Top Namespace Group Times:
------------------------------------------------------------
Slim.*                                   (39,2913 ms)(5 files)
Test.*                                   (19,1163 ms)(4 files)
------------------------------------------------------------
Total time  : 111,6262 ms
Median time : 5,9196 ms
```

## Example Output with FilterMask

When using a filter mask, the output will include a "Mask time" line:

`DcuCompileTimes.exe "slim.*"`

```console
------------------------------------------------------------
Files, sorted by generation time (ms), factor to median (x):
------------------------------------------------------------
Slim.Fixture.dcu                         (16,0846 ms) (x2,72)
Slim.Exec.dcu                            (6,3514 ms) (x1,07)
Slim.Common.dcu                          (5,9580 ms) (x1,01)
Slim.List.dcu                            (5,8812 ms) (x0,99)
Slim.Symbol.dcu                          (5,0161 ms) (x0,85)
TestInsight.Client.dcu <- build start 09.11.2025 19:09:21
------------------------------------------------------------
Top Namespace Group Times:
------------------------------------------------------------
Slim.*                                   (39,2913 ms)(5 files)
------------------------------------------------------------
Total time  : 111,6262 ms
Median time : 5,9196 ms
Mask matches: 5
Mask time   : 39,2913 ms (35,20%)
```

