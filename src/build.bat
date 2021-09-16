ifort -c cinmod.f
ifort -c convert.f90
ifort -c culdmp.f
ifort -c culrat.f
ifort -c culvert.f -I ..\sdk\includes
ifort -c discharg.f
ifort -c ellip.f
ifort -c icvprp.f
ifort -c icvprp_cgns.f
ifort -c ixsprp.f
ifort -c ixsprp_cgns.f
ifort -c pipearc.f
ifort -c shydie.f
ifort -c shydie_cgns.f
ifort -o CAP.exe *.obj ..\sdk\libs\cgnsdll.lib ..\sdk\libs\iriclib.lib
