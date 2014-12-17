@echo off
rem color f0
rem chcp 1251
rem tasklist /nh |find /i "acad.exe"
rem if %errorlevel%==1 goto ookay
rem chcp 866
rem echo AutoCAD ‡€“™…!!!
rem echo ‡€Š‰ AutoCAD ˆ “‰ ‘‚€!!!
rem color fc
rem goto end
rem :ookay
color f2
rem for /f "tokens=2*" %%a in ('reg query HKLM\SOFTWARE\Autodesk\AutoCAD\R17.1\ACAD-6000:419 /v AcadLocation') do set acaddir=%%b
chcp 1251
for /f "tokens=2*" %%a in ('reg query "HKEY_CURRENT_USER\Volatile Environment" /v AppData') do set userdat=%%b
del "%userdat%\geo_tools\version.txt" /Q
xcopy "%userdat%\geo_tools\data" "%systemdrive%\Program Files\geo_tools\data" /E /Y /R /H /Q
xcopy "%userdat%\geo_tools\template" "%systemdrive%\Program Files\geo_tools\template" /E /Y /R /H /Q
echo %date:~6,4%%date:~3,2%%date:~0,2%> "%userdat%\geo_tools\version.txt"
chcp 866
set mydir="%systemdrive%\geo_tools"
md %mydir%
del "%systemdrive%\Program Files\geo_tools\version.txt" /Q
echo %date:~6,4%%date:~3,2%%date:~0,2%%time:~0,2%%time:~3,2%> "%systemdrive%\Program Files\geo_tools\version.txt"
7z a -tzip -mx=9 "%systemdrive%\geo_tools\geo_tools.zip" "%systemdrive%\Program Files\geo_tools"
ren "%mydir%\geo_tools.zip" "geo_tools %date:~6,4%.%date:~3,2%.%date:~0,2% %time:~0,2%-%time:~3,2%.zip"
ren "%mydir%" "geo_tools (%date:~6,4%.%date:~3,2%.%date:~0,2% %time:~0,2%-%time:~3,2%)"
echo “€Š‚Š€ ‡€‚…˜…€!!!
rem :end
rem echo on
@pause