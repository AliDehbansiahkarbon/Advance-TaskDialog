@echo off 
ECHO Delete AxTaskDialog Resource file ...
Del "AxTaskDialogResource.RES"
pause
ECHO -------------------------------------------------------------------------------
ECHO Create AxTaskDialog Resource file ...
brcc32.exe "AxTaskDialogResource.rc"
pause