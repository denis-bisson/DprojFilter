::********************************************************************************
::* DprojFilter                                                                  *
::* -----------------------------------------------------------------------------*
::* Batch file to clean the project directory.                                   *
::* May be executed prior build, commmit, backup, etc. to remove unused files.   *
::* Written by Denis Bisson, Drummondville, Québec, 2021-08-27.                  *
::* -----------------------------------------------------------------------------*
::* Used in the project DprojFilter                                              *
::* Originally and mainly written by Thomas Mueller                              *
::*   https://osdn.net/projects/dprojfilter                                      *
::* This little adaptation written by Denis Bisson, Drummondville, Québec, Canada*
::*   https://github.com/denis-bisson/DprojFilter                                *
::*   2021-08-27                                                                 *
::* -----------------------------------------------------------------------------*
::* You should not remove these comments.                                        *
::********************************************************************************
::*
@echo off
echo "Cleaning files..."
rd /Q /S __recovery
rd /Q /S __history
rd /Q /S dzlib\src\__recovery
rd /Q /S dzlib\src\__history
rd /Q /S dzlib\jedi_inc\__recovery
rd /Q /S dzlib\jedi_inc\__history
rd /Q /S dzlib\templates\__recovery
rd /Q /S dzlib\templates\__history
rd /Q /S RegExLib\__recovery
rd /Q /S RegExLib\__history
rd /Q /S dcu
del /Q /S *.local
del /Q /S *.res
del /Q /S *.map
del /Q /S *.drc
del /Q /S *.stat
del /Q /S *.local
del /Q /S *.~dsk
del /Q /S *.identcache
del /Q /S *.dsk
del DprojFilter.exe
echo "Clean is done!"