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
DcuCompileTimes.exe [FilterMask]

Analyzes Delphi .dcu compilation times in the current directory.

Arguments:
  [FilterMask]  Optional. A file mask (e.g., "Base.*", "Base.Db.*") to filter
                the displayed files and calculate a "Mask time" summary.

Options:
  --help, -h, ? Displays this help message.
```

## Example Output

```console
------------------------------------------------------------
Files, sorted by generation time (ms), factor to median (x):
------------------------------------------------------------
WDDT.DelayedMethod.dcu                   (49,0252 ms) (x7,60)
Test.SlimSymbol.dcu                      (17,4356 ms) (x2,70)
Slim.Fixture.dcu                         (11,9609 ms) (x1,85)
Test.SlimExec.dcu                        (8,9239 ms) (x1,38)
Slim.Common.dcu                          (7,0074 ms) (x1,09)
TestInsight.DUnitX.dcu                   (6,9073 ms) (x1,07)
Test.SlimFixture.dcu                     (6,0005 ms) (x0,93)
Slim.Exec.dcu                            (4,9979 ms) (x0,77)
Slim.List.dcu                            (4,9296 ms) (x0,76)
Slim.Symbol.dcu                          (4,0528 ms) (x0,63)
Test.SlimList.dcu                        (2,0642 ms) (x0,32)
------------------------------------------------------------
Total time : 123,3053 ms
Median time: 6,4539 ms
```

## Example Output with FilterMask

When using a filter mask, the output will include a "Mask time" line:

`DcuCompileTimes.exe "slim.*"`

```console
------------------------------------------------------------
Files, sorted by generation time (ms), factor to median (x):
------------------------------------------------------------
Slim.Fixture.dcu                         (11,9609 ms) (x1,85)
Slim.Common.dcu                          (7,0074 ms) (x1,09)
Slim.Exec.dcu                            (4,9979 ms) (x0,77)
Slim.List.dcu                            (4,9296 ms) (x0,76)
Slim.Symbol.dcu                          (4,0528 ms) (x0,63)
------------------------------------------------------------
Total time : 123,3053 ms
Median time: 6,4539 ms
Mask time  : 32,9486 ms (26,72%)
```

