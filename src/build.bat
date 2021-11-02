@REM Get iricsdk-4.x.x.7z from https://github.com/i-RIC/iriclib_v4/releases/latest
@del *.obj *.mod CAP.exe
@ifort -nologo -c ixsprp.f
@ifort -nologo -c convert.f90
@ifort -nologo -c shydie.f
@ifort -nologo -c pipearc.f
@ifort -nologo -c ellip.f
@ifort -nologo -c icvprp.f
@ifort -nologo -c culrat.f
@ifort -nologo -c ixsprp_cgns.f
@ifort -nologo -c discharg.f
@ifort -nologo -c culdmp.f
@ifort -nologo -c ../iricsdk/iriclib-%IRICLIB_VER%/src/iric.f90
@ifort -nologo -c shydie_cgns.f
@ifort -nologo -c icvprp_cgns.f
@ifort -nologo -c culvert.f
@ifort -nologo -c cinmod.f
@ifort -nologo -o CAP.exe *.obj ../iricsdk/iriclib-%IRICLIB_VER%/lib/iriclib.lib
