@echo off
set stdin=%2.stdin
if exist %2.out del %2.out
echo %2.dat > %stdin%
echo %2.out >> %stdin%
echo %~3 >> %stdin%
%1 < %stdin%
REM if exist %stdin% del %stdin%
echo cmake -E compare_files %2.out.expected %2.out
cmake -E compare_files %2.out.expected %2.out
exit %ERRORLEVEL%
