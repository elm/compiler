; Elm Installer

;--------------------------------
;Includes

  !Include "FileFunc.nsh"
  !Include "LogicLib.nsh"
  !Include "MUI2.nsh"
  !Include "WordFunc.nsh"
  !Include "CreateInternetShortcut.nsh"

;--------------------------------
;Defines

  !Define PRODUCT_DIR_REG_KEY "Software\Elm\Elm\${PLATFORM_VERSION}"
  !Define FILES_SOURCE_PATH "files"
  !Define INST_DAT "inst.dat"
  !Define UNINST_DAT "uninst.dat"

;--------------------------------
;Variables

  Var START_MENU_FOLDER

;--------------------------------
;General settings

  ;Name and file
  Name "Elm ${PLATFORM_VERSION}"
  OutFile "Elm-${PLATFORM_VERSION}.exe"

  ;Default install dir
  InstallDir "$PROGRAMFILES\Elm\${PLATFORM_VERSION}"
  InstallDirRegKey HKLM "${PRODUCT_DIR_REG_KEY}" ""

  ;Icon
  !Define MUI_ICON "logo.ico"
  !Define MUI_UNICON "logo.ico"

  ;Request application privileges for Windows Vista
  RequestExecutionLevel highest

  ;Best available compression
  SetCompressor /SOLID lzma

  ;Install types
  InstType "Standard"
  InstType "Portable (just unpack the files)"

;--------------------------------
;Macros

!macro CheckAdmin thing
UserInfo::GetAccountType
pop $0
${If} $0 != "admin" ;Require admin rights on NT4+
    MessageBox MB_YESNO "It is recommended to run this ${thing} as administrator. Do you want to quit and restart the ${thing} manually with elevated privileges?" IDNO CheckAdminDone
    SetErrorLevel 740 ;ERROR_ELEVATION_REQUIRED
    Quit
${EndIf}
CheckAdminDone:
!macroend

;--------------------------------
;Callbacks

Function .onInit
  !insertmacro CheckAdmin "installer"
  SetShellVarContext all
FunctionEnd

Function un.onInit
  !insertmacro CheckAdmin "uninstaller"
  SetShellVarContext all
FunctionEnd

Function LaunchLink
  ExecShell "open" "http://elm-lang.org/get-started"
FunctionEnd

;--------------------------------
;Interface Settings

  !define MUI_ABORTWARNING

;--------------------------------
;Pages

  !Define MUI_WELCOMEFINISHPAGE_BITMAP "welcome.bmp"
  !insertmacro MUI_PAGE_WELCOME
  ;!insertmacro MUI_PAGE_LICENSE "LICENSE"
  !insertmacro MUI_PAGE_DIRECTORY

  !Define MUI_COMPONENTSPAGE_NODESC
  !insertmacro MUI_PAGE_COMPONENTS

  ;Start Menu Folder Page Configuration
  !Define MUI_PAGE_HEADER_SUBTEXT \
  "Choose a Start Menu folder for the Elm ${PLATFORM_VERSION} shortcuts."
  !Define MUI_STARTMENUPAGE_TEXT_TOP \
  "Select the Start Menu folder in which you would like to create Elm shortcuts. You can also enter a name to create a new folder."
  !Define MUI_STARTMENUPAGE_REGISTRY_ROOT "HKLM"
  !Define MUI_STARTMENUPAGE_REGISTRY_KEY "${PRODUCT_DIR_REG_KEY}"
  !Define MUI_STARTMENUPAGE_REGISTRY_VALUENAME "Start Menu Folder"
  !Define MUI_STARTMENUPAGE_DEFAULTFOLDER "Elm ${PLATFORM_VERSION}"
  !insertmacro MUI_PAGE_STARTMENU StartMenuPage $START_MENU_FOLDER
  !insertmacro MUI_PAGE_INSTFILES
  !define MUI_FINISHPAGE_RUN
  !define MUI_FINISHPAGE_RUN_FUNCTION "LaunchLink"
  !define MUI_FINISHPAGE_RUN_TEXT "Open tutorial on how to use Elm"
  !insertmacro MUI_PAGE_FINISH

  !insertmacro MUI_UNPAGE_WELCOME
  !insertmacro MUI_UNPAGE_CONFIRM
  !insertmacro MUI_UNPAGE_INSTFILES
  !insertmacro MUI_UNPAGE_FINISH

;--------------------------------
;Languages

  !insertmacro MUI_LANGUAGE "English"

;--------------------------------
;Installer Sections

Section "Base components" SecMain

  SectionIn 1 2
  ; Make this section mandatory
  SectionIn RO

  !Include ${INST_DAT}

SectionEnd

SectionGroup "Update system settings" SecGr

Section "Associate with .elm files" SecAssoc

  SectionIn 1

  ; File associations
  WriteRegStr HKCR ".elm" "" "elm"
  WriteRegStr HKCR "elm" "" "Elm Source File"
  WriteRegStr HKCR "elm\DefaultIcon" "" "$INSTDIR\file.ico"
  ;WriteRegStr HKCR "elm\shell\open\command" "" '"$INSTDIR\bin\elm.exe" "%1"'

  ;Remember that we registered associations
  WriteRegDWORD HKLM "${PRODUCT_DIR_REG_KEY}" Assocs 0x1

SectionEnd

Section "Update the PATH environment variable" SecPath

  SectionIn 1

  ; Update PATH
  ; First, remove any older version
  ExecWait '"$SYSDIR\wscript.exe" //E:vbscript "$INSTDIR\removefrompath.vbs" "$PROGRAMFILES\Elm"'
  ; Then add to the PATH
  ExecWait '"$SYSDIR\wscript.exe" //E:vbscript "$INSTDIR\updatepath.vbs" "$INSTDIR\bin"'
  SetShellVarContext current

  ; Update environment variables
  SendMessage ${HWND_BROADCAST} ${WM_SETTINGCHANGE} 0 "STR:Environment" /TIMEOUT=5000

SectionEnd

Section "Store Elm's location in registry" SecElmLoc

  SectionIn 1

  ; (copied from the GHC installer).
  ;WriteRegStr HKCU "Software\Elm\ghc-${GHC_VERSION}" "InstallDir" "$INSTDIR"
  WriteRegStr HKCU "Software\Elm" "InstallDir" "$INSTDIR"

SectionEnd

Section "Create uninstaller" SecAddRem

  SectionIn 1
  SectionIn RO

  ; Add uninstall information to Add/Remove Programs
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Elm-${PLATFORM_VERSION}" \
  "DisplayName" "Elm ${PLATFORM_VERSION}"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Elm-${PLATFORM_VERSION}" \
  "UninstallString" "$\"$INSTDIR\Uninstall.exe$\""
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Elm-${PLATFORM_VERSION}" \
  "DisplayIcon" "$INSTDIR\logo.ico"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Elm-${PLATFORM_VERSION}" \
  "Publisher" "elm-lang.org"

  ;Create uninstaller
  WriteUninstaller "$INSTDIR\Uninstall.exe"

  ; This is needed for uninstaller to work
  WriteRegStr HKLM "${PRODUCT_DIR_REG_KEY}" "" "$INSTDIR\Uninstall.exe"
  WriteRegStr HKLM "${PRODUCT_DIR_REG_KEY}" "InstallDir" "$INSTDIR"

SectionEnd

SectionGroupEnd

;Section "-StartMenu" StartMenu
;  SectionIn 1 2
;
;  ; Add start menu shortcuts
;
;  !insertmacro MUI_STARTMENU_WRITE_BEGIN StartMenuPage
;
;    ;Create shortcuts
;    CreateDirectory "$SMPROGRAMS\$START_MENU_FOLDER"
;    !insertmacro CreateInternetShortcut \
;    "$SMPROGRAMS\$START_MENU_FOLDER\${HACKAGE_SHORTCUT_TEXT}" \
;    "http://hackage.haskell.org" \
;    "$INSTDIR\icons\hackage.ico" "0"
;  !insertmacro MUI_STARTMENU_WRITE_END
;
;SectionEnd

;--------------------------------
;Uninstaller Section

Section "Uninstall"

  ; Update PATH
  ExecWait '"$SYSDIR\wscript.exe" //E:vbscript "$INSTDIR\removefrompath.vbs" "$PROGRAMFILES\Elm"'
  SetShellVarContext current

  !Include ${UNINST_DAT}

  Delete "$INSTDIR\Uninstall.exe"
  RMDir $INSTDIR

  ;Since we install to '$PF\Elm\$PLATFORM_VERSION', we
  ;should also try to delete '$PF\Elm' if it is empty.
  ${GetParent} $INSTDIR $R0
  RMDir $R0

  ; Delete start menu shortcuts
  ;!insertmacro MUI_STARTMENU_GETFOLDER StartMenuPage $START_MENU_FOLDER

  ;Delete "$SMPROGRAMS\$START_MENU_FOLDER\${HACKAGE_SHORTCUT_TEXT}.url"
  ;RMDir "$SMPROGRAMS\$START_MENU_FOLDER\"

  ; Delete registry keys

  ReadRegDWORD $0 HKLM "${PRODUCT_DIR_REG_KEY}" Assocs

  ${If} $0 = 0x1
    DeleteRegValue HKCR ".elm" ""
    DeleteRegKey HKCR "elm\DefaultIcon"
  ${EndIf}

  DeleteRegKey HKCU "Software\Elm"
  DeleteRegKey HKLM "${PRODUCT_DIR_REG_KEY}"
  DeleteRegKey /IfEmpty HKCU Software\Elm
  DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Elm-${PLATFORM_VERSION}"

  ; Update environment variables
  SendMessage ${HWND_BROADCAST} ${WM_SETTINGCHANGE} 0 "STR:Environment" /TIMEOUT=5000

SectionEnd