@echo off
rem sed -e "s/BuildDate As String = \"[\.\+\:a-zA-Z0-9]*\"/BuildDate As String = \"\"/g" -e "s/BuildRev As String = \"[\.\+\:a-zA-Z0-9]*\"/BuildRev As String = \"\"/g" -e "s/BuildTime As String = \"[\.\+\:a-zA-Z0-9]*\"/BuildTime As String = \"\"/g" "Fractals.Cli\Main.vb" -i

rem sed -e "s/BuildDate As String = \"[\.\+\:a-zA-Z0-9]*\"/BuildDate As String = \"\"/g" -e "s/BuildRev As String = \"[\.\+\:a-zA-Z0-9]*\"/BuildRev As String = \"\"/g" -e "s/BuildTime As String = \"[\.\+\:a-zA-Z0-9]*\"/BuildTime As String = \"\"/g" "Fractals.GUI\MainWindow.xaml.vb" -i

if EXIST Fractals.Cli\Main.vb.orig mv Fractals.Cli\Main.vb.orig Fractals.Cli\Main.vb
if EXIST Fractals.GUI\MainWindow.xaml.vb.orig mv Fractals.GUI\MainWindow.xaml.vb.orig Fractals.GUI\MainWindow.xaml.vb