@echo off
%1 Case1.cgn > consoleLog.txt 2>&1
echo cmake -E compare_files consoleLog.txt.expected consoleLog.txt
cmake -E compare_files consoleLog.txt.expected consoleLog.txt
exit %ERRORLEVEL%
