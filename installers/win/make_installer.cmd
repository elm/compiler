
set version=%1

runhaskell ..\BuildFromSource.hs %version%
if %errorlevel% neq 0 exit /b %errorlevel%

mkdir files
mkdir files\bin

set platform=Elm-Platform\%version%

xcopy %platform%\.cabal-sandbox\bin\elm*.exe files\bin /s /e
xcopy updatepath.vbs files

if EXIST "%ProgramFiles%" (
    set nsis=%ProgramFiles%\NSIS
) else (
    set nsis=%ProgramFiles(x86)%\NSIS
)

"%nsis%\makensis.exe" /DPLATFORM_VERSION=%version% Nsisfile.nsi

rd /s /q files
