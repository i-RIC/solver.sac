@echo off
cls
set topdir=..
set data=%topdir%\data\
set prog=%topdir%\bin\sac.exe
echo.
echo.
echo Five tests of SAC will be performed using data from the
echo ..\data directory.  Information will be sent to the screen which
echo you may ignore, unless the tests abort with an error message.
echo.
echo Note: the data directory contains output files of the tests
echo that can be compared with the output files generated during
echo the test (DO NOT ALTER files in the ..\data directory).
echo.
pause
if exist example1.out del example1.out
if exist example2.out del example2.out
if exist example3.out del example3.out
if exist example4.out del example4.out
if exist example5.out del example5.out
if exist go del go
echo.
echo Slope-area reach example with level-water surface in cross sections
echo.
echo %data%example1.dat >> go
echo example1.out >> go
echo Slope-area reach example with level-water surface in cross sections >> go
%prog% < go
echo.
echo First test completed
echo.
echo Slope-area reach: Snake Creek near Connell, WA for flood of Feb. 21, 1956
echo.
del go
echo %data%example2.dat >> go
echo example2.out >> go
echo Slope-area reach: Snake Creek near Connell, WA for flood of Feb. 21, 1956 >> go
%prog% < go
echo.
echo Second test completed
echo.
echo Sloping water surface with conveyance weighting
echo.
del go
echo %data%example3.dat >> go
echo example3.out >> go
echo Sloping water surface with conveyance weighting >> go
%prog% < go
echo.
echo Third test completed
echo.
echo Slope-area reach example with level-water surface in cross sectionsc
echo.
del go
echo %data%example4.dat >> go
echo example4.out >> go
echo Slope-area reach example with level-water surface in cross sections, metric output >> go
%prog% < go
echo.
echo Fourth test completed
echo.
echo Simple reach example with metric input and feet-second output
echo.
del go
echo %data%example5.dat >> go
echo example5.out >> go
echo Simple reach example with metric input and feet-second output >> go
%prog% < go
echo.
echo Finished all tests
pause
