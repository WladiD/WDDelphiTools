@echo off
setlocal

set "PORT=%1"
if "%PORT%"=="" set "PORT=9012"

echo Connecting to IDE on port %PORT%...

rem Check if port is listening (simple check via netstat or similar could be done, 
rem but Slim runner will handle connection failures).

rem We need to keep this process alive while FitNesse runs the tests.
rem FitNesse connects to the port independently.
rem This script mimics the "Server Process".

:LOOP
timeout /t 1 > nul
rem Check if IDE is still running?
rem For now just loop until killed by FitNesse.
rem goto LOOP
