@echo off
Set PREVDIR=%CD%

cd %~dp0

for /f "delims=" %%x in (../etc/dbuddy.cfg) do (set "%%x")

cd ../

cd ./lib

Rscript exec.R %*

cd %PREVDIR%
