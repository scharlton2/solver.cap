C=====CULDMP bof=============================================================
C
C-----Purpose:
C       dumps culvrt.inc commons to asii file for test and review
C     Programmed by: JM Fulford
C
      SUBROUTINE CULDMP(OTNIT)
C
C-----Arguments:
      INTEGER OTNIT
C
C-----Module data:
      INCLUDE 'CULVRT.INC'
C
C-----Local variables:
      INTEGER I
C
C-----Formats:
C  104 FORMAT (1X,'BARREL DEPTH',15X,'AREA',7X,'CONVEYANCE',6X,'TOP
C     1WIDTH',3X,'WETTED PERIMETER')
C  105 FORMAT (3X,F8.3,11X,F10.2,5X,F11.1,5X,F10.2,5X,F10.2)
C  121 FORMAT(30X,'CULVERT BARREL PROPERTIES')
  100 FORMAT('TBSIZE = ',I4,' MXCOEF = ',I4)
  101 FORMAT('Culvert ID - ',A16,' Culvert type  - ',I3,'  N - ',F5.3,/)
  102 FORMAT('  Discharge coefficients',/,
     # ' CB12  C46   Kr    Kw    Kphi'
     # ,/,1X,5F5.2)
  103 FORMAT(/,'Table discharge coefficients CP')
  114 FORMAT(8F5.2,/)
  115 FORMAT(/,'Table discharge coefficients C5')
  116 FORMAT(/,'Pipe arch corner radii bottom top corner',3F5.1)
C
      WRITE(OTNIT,100)TBSIZE,MXCOEF
      WRITE(OTNIT,101)VRTID,TC,NRUFF
      WRITE(OTNIT,102)CB12,C46,KR,KW,KWING
      WRITE(OTNIT,103)
      WRITE(OTNIT,114)(HP(I),CP(I),I=1,MXCOEF)
      WRITE(OTNIT,115)
      WRITE(OTNIT,114)(H5(I),C5(I),I=1,MXCOEF)
      WRITE(OTNIT,116)(ARAD(I),I=1,3)
C      CALL WCULTB(OTNIT)
C
      RETURN
      END
C
C=====CULDMP eof=============================================================
C
C=====DMPCUL bof=============================================================
C
C-----Purpose:
C       dumps contents of FCNCN2.INC common block
C     Programmed by: JM Fulford
C
      SUBROUTINE DMPCUL(OTNIT)
C
C-----Arguments:
      INTEGER OTNIT
C
C-----Argument definition:
C     OTNIT  - file unit number for dump
C
C-----Module data:
      INCLUDE 'FCNCN2.INC'
C
C-----Formats:
100   FORMAT(1X,'Z=',F5.2,' DC=',F5.2,' DCPZ=',F5.2,' DCMZ=',F5.2)
101   FORMAT(1X,'VGDC=',F5.2,' H12DC=',F5.2,' H23DC=',F5.2,' ADC=',
     #       F6.1)
102   FORMAT(1X,'D3=',F6.1,' VGD3=',F5.2,' H23D3=',F5.2,' AD3=',F6.1)
103   FORMAT(1X,'H12D2=',F5.2,' QG=',F8.1,' CC=',F5.2)
C
      WRITE(OTNIT,100) Z,DC,DCPZ,DCMZ
      WRITE(OTNIT,101) VGDC,H12DC,H23DC,ADC
      WRITE(OTNIT,102) D3,VGD3,H23D3,AD3
      WRITE(OTNIT,103) H12D2,QG,CC
C
      RETURN
      END
C
C=====DMPCUL Eof=============================================================
C
C=====CULRD bof=============================================================
C
C-----Purpose:
C       return location of cross section, the reference distance
C     Programmed by: JM Fulford
C
      REAL FUNCTION CULRD(CLID)
C
C     Arguments:
      CHARACTER*16 CLID
C
C-----Module data:
      INCLUDE 'CULVRT.INC'
C
C-----Externals:
      LOGICAL IDMATCH
      EXTERNAL IDMATCH
C
      IF(.NOT.IDMATCH(CLID,VRTID)) THEN
        WRITE(*,*)'ERROR IN CULRD'
      ENDIF
      CULRD=CLSRD
C
      RETURN
      END
C
C=====CULRD eof=============================================================
C
C=====APPRD Bof=============================================================
C
C-----Purpose:
C       return location of approach section, the reference distance
C     Programmed by: JM Fulford
C
      REAL FUNCTION APPRD(XSCID)
C
C-----Arguments:
      CHARACTER*16 XSCID
C
C-----Module data:
      INCLUDE 'APPROC.INC'
C
C-----Externals:
      LOGICAL IDMATCH
      EXTERNAL IDMATCH
C
      IF(.NOT.IDMATCH(XSCID,APPID))THEN
        WRITE(*,*)'ERROR IN APPRD',XSCID,APPID
      ENDIF
      APPRD=APSRD
C
      RETURN
      END
C
C=====APPRD eof=============================================================
C
C=====CULLEN Bof=============================================================
C
C
C-----Purpose:
C       returns the culvert length of clid
C     Programmed by: JM Fulford
C
      REAL FUNCTION CULLEN(CLID)
C
C-----Arguments:
      CHARACTER*16 CLID
C
C-----Argument definition:
C     CLID  -  culvert id
C
C-----Module data:
      INCLUDE 'CULVRT.INC'
C
C-----Externals:
      LOGICAL IDMATCH
      EXTERNAL IDMATCH
C
      IF(.NOT.IDMATCH(CLID,VRTID)) THEN
        WRITE(*,*)'ERROR IN CULRD',CLID,VRTID
      ENDIF
      CULLEN=CLEN
C
      RETURN
      END
C
C=====CULLEN eof=============================================================
C
C=====FLWDMP Bof=============================================================
C
C
C-----Purpose:
C       dumps the culflw common for review and testing
C     Programmed by: JM Fulford
C
      SUBROUTINE FLWDMP(OTNIT)
C
C-----Arguments:
      INTEGER OTNIT
C
C-----Module data:
      INCLUDE 'CULFLW.INC'
C
C-----Local variables:
      INTEGER I
C
C-----Formats:
100   FORMAT('MAXNO-',I4,' NQ-',I3,' NTW-',I3)
101   FORMAT('DISCHARGES',/,6F7.1)
102   FORMAT('TAILWATERS',/,6F7.2)
104   FORMAT(I3,1X,2F7.2,I3,1X,3F7.2,I3)
C
      WRITE(OTNIT,100)MAXNO,NQ,NTW
      WRITE(OTNIT,101)(Q(I),I=1,NQ)
      WRITE(OTNIT,102)(H4TW(I),I=1,NTW)
      WRITE(OTNIT,*)'NUMBER OF CALS ',INUM
      IF(INUM.GT.0)THEN
         DO 10 I=1,INUM
         WRITE(OTNIT,104)I,H1E(I),H4E(I),TYPE(I),D2(I),D3A(I),PKON(I)
     #    ,ERRCOD(I)
 10   CONTINUE
      ENDIF
C
      RETURN
      END
C
C=====FLWDMP Eof=============================================================
