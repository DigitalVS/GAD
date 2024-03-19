# bin2gtp

`bin2gtp` is program that converts assembly BIN files to GTP format. It is originally written for Linux OS by Tomaž Šolc. In this version it is ported to Windows and modified to allow wrapping binary files with no BASIC part of code, into the GTP file. Full list of program options may be displayed by issuing `bin2gtp.exe -h`. Command format used to transfer GAD bin file to GTP is:

```
bin2gtp.exe -n GAD -a 0x4800 debugger.bin
```
where `0x4800` is GAD start address and should be set to the same value as source code `.org` assembler directive.

Program requires installed Microsoft Visual C++ Redistributable runtime libraries (version 14 for Visual Studio 2015, 2017, 2019, and 2022).