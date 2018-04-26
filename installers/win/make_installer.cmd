
set version=%1

mkdir files
mkdir files\bin

xcopy ..\..\dist\build\elm\elm.exe files\bin /s /e
xcopy updatepath.vbs files

if EXIST "%ProgramFiles%\NSIS" (
    set nsis="%ProgramFiles%\NSIS\makensis.exe"
) else (
    set nsis="%ProgramFiles(x86)%\NSIS\makensis.exe"
)

%nsis% /DPLATFORM_VERSION=%version% Nsisfile.nsi

rd /s /q files
