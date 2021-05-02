all: x64 x86
clean: 
	@for /D %%f in (.\*) do (rm -rf %%f\obj\)
	@for /D %%g in (.\*) do (rm -rf %%g\bin\)
	
	@rm -f Fractals.6.0.ReSharper.user
	@.\Scripts\DeleteVersion.bat
	@.\Scripts\headers.bat
x64:
	@"%windir%\Microsoft.NET\Framework\v4.0.30319\MSBuild.exe" /m:6 /p:Configuration=Release /p:Platform=x64
x86:
	@"%windir%\Microsoft.NET\Framework\v4.0.30319\MSBuild.exe" /m:6 /p:Configuration=Release /p:Platform=x86
help:
	@echo Available Targets:
	@echo  x86   : Build Setup for x86 and the rest for AnyCPU
	@echo  x64   : Build Setup for x64 and the rest for AnyCPU
	@echo  all   : Build x86 and x64
	@echo  clean : Delete all binary and intermediate output files
	@echo  help  : List available targets
.PHONY: clean