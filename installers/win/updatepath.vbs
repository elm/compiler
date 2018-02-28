Set WshShell = CreateObject("WScript.Shell")
elmPath = WScript.Arguments(0)
'const PathRegKey = "HKLM\SYSTEM\CurrentControlSet\Control\Session Manager\Environment\Path"
const PathRegKey = "HKCU\Environment\Path"

on error resume next
path = WshShell.RegRead(PathRegKey)
if err.number <> 0 then
	path = ""
end if
on error goto 0

newPath = elmPath & ";" & path
Call WshShell.RegWrite(PathRegKey, newPath, "REG_EXPAND_SZ")
