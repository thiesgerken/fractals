@echo off
hg id | sed -e "s/ tip//g" > rev
date /T > date 
time /T > time

set /p date= < .\date
set /p time= < .\time
set /p rev= < .\rev

rm rev
rm date
rm time

sed -e "s/BuildDate As String = \"[a-zA-Z0-9]*\"/BuildDate As String = \"%date%\"/g" -e "s/BuildRev As String = \"[a-zA-Z0-9]*\"/BuildRev As String = \"%rev%\"/g" -e "s/BuildTime As String = \"[a-zA-Z0-9]*\"/BuildTime As String = \"%time%\"/g" "Fractals.Cli\Main.vb" -i.orig

sed -e "s/BuildDate As String = \"[a-zA-Z0-9]*\"/BuildDate As String = \"%date%\"/g" -e "s/BuildRev As String = \"[a-zA-Z0-9]*\"/BuildRev As String = \"%rev%\"/g" -e "s/BuildTime As String = \"[a-zA-Z0-9]*\"/BuildTime As String = \"%time%\"/g" "Fractals.GUI\MainWindow.xaml.vb" -i.orig

rm .\sed*