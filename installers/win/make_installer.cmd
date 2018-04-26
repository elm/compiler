
set version=%1

git clone https://github.com/elm-lang/elm-compiler.git
cd elm-compiler
git checkout %version%
cabal sandbox init
cabal update
cabal configure
cabal build

if %errorlevel% neq 0 exit /b %errorlevel%

mkdir files
mkdir files\bin

xcopy dist/build/elm/elm.exe files\bin /s /e
xcopy updatepath.vbs files

if EXIST "%ProgramFiles%" (
    set nsis=%ProgramFiles%\NSIS
) else (
    set nsis=%ProgramFiles(x86)%\NSIS
)

"%nsis%\makensis.exe" /DPLATFORM_VERSION=%version% Nsisfile.nsi

rd /s /q files
