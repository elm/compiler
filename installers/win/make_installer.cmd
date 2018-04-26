
set version=%1

mkdir files
mkdir files\bin

xcopy ..\..\dist\build\elm\elm.exe files\bin /s /e
xcopy updatepath.vbs files

if EXIST "%ProgramFiles%" (
    set nsis=%ProgramFiles%\NSIS
) else (
    set nsis=%ProgramFiles(x86)%\NSIS
)

"%nsis%\makensis.exe" /DPLATFORM_VERSION=%version% Nsisfile.nsi

rd /s /q files
