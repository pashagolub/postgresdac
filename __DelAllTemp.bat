@echo off

IF EXIST *.~* del /Q /S *.~* > nul
IF EXIST *.dcu del /Q /S *.dcu > nul
IF EXIST *.stat del /Q /S *.stat > nul
IF EXIST *.dof del /Q /S *.dof > nul
IF EXIST *.hpp del /Q /S *.hpp > nul
IF EXIST *.tds del /Q /S *.tds > nul
IF EXIST *.obj del /Q *.obj > nul
IF EXIST *.exe del /Q /S *.exe > nul
IF EXIST *.map del /Q /S *.map > nul
IF EXIST *.mps del /Q /S *.mps > nul
IF EXIST *.mpt del /Q /S *.mpt > nul
IF EXIST *.dsk del /Q /S *.dsk > nul
IF EXIST *.drc del /Q /S *.drc > nul
IF EXIST *.elf del /Q /S *.elf > nul
IF EXIST *.ddp del /Q /S *.ddp > nul
IF EXIST *.bak del /Q /S *.bak > nul
IF EXIST *.bpl del /Q /S *.bpl > nul
IF EXIST *.lib del /Q /S *.lib > nul
IF EXIST *.dcp del /Q /S *.dcp > nul
IF EXIST *.bpi del /Q /S *.bpi > nul
IF EXIST *.ilc del /Q /S *.ilc > nul
IF EXIST *.ild del /Q /S *.ild > nul
IF EXIST *.ilf del /Q /S *.ilf > nul
IF EXIST *.ils del /Q /S *.ils > nul
IF EXIST *.identcache del /Q /S *.identcache > nul
IF EXIST *.bdsproj.local del /Q /S *.bdsproj.local > nul
IF EXIST __history del /Q /S __history > nul
IF EXIST __history rmdir __history > nul