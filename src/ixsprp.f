C=====IXSGEO bof=======================================================
C
C-----Purpose:
C       open wspro file containing approach geometry and return the unit
C       no. on which the file is openned and close file.
C     Programmed by: JM Fulford
C     Date:
C     Modified by: JM Fulford
C     Last modified:  Dec 28, 2011, changed to allow command line input of file names
C       section ids and file header text and the older DOS interactive user inputs
C       expanded the string size allowed (APPROX.INC) for XSNAM to 132 char
C     Modified by gfkoltun for 2021 version      
C
      LOGICAL FUNCTION IXSGEO()
C
C      IMPLICIT NONE
C
C-----Module data:
      INCLUDE 'APPROC.INC'
C
C-----Local variables:
C
C-----Argument definitions:
C     ERR    - error flag
C
      ERR=1
      IXSGEO=.FALSE.
      XSNIT=12
c     code modified may 27, 2011 to allow all file names and ids for culvert and approach
c     section to be input on command line. Does not trap errors.  Added GETARG calls.
      CALL GETARG(4,XSNAM)
      IF(LEN_TRIM(XSNAM).GT.0)THEN
        OPEN(UNIT=XSNIT,FILE=XSNAM,STATUS='OLD',ERR=300)
        IXSGEO=.TRUE.
        ERR=0
      ELSE
        WRITE(*,*)'Enter file name containing approach sect. geometry:'
 200    CONTINUE
        READ(*,'(A)') XSNAM
        IF(XSNAM(1:1).EQ.'#') GOTO 300
        WRITE(*,*) 'file requested is '//TRIM(XSNAM) 
C       ERR statement below used to point to 300, changed to 100 to give
C       the user additional chances to correctly specify name. (gfk)
        OPEN(UNIT=XSNIT,FILE=XSNAM,STATUS='OLD',ERR=100)
        IXSGEO=.TRUE.
        ERR=0  
 100    CONTINUE
        IF(ERR.EQ.1) THEN
          WRITE(*,*) TRIM(XSNAM)//' not found: enter filename or # to qu
     #it'
          GO TO 200
        ENDIF
      ENDIF
 300  CONTINUE
      CLOSE (UNIT=XSNIT)
C
      RETURN
      END
C
C=====IXSGEO eof==============================================================
C
C=====IXSPRP bof==============================================================
C
C-----Purpose:
C       initialize approach section properties table
C     Programmed by: JM Fulford
C     Date:
C     Modified by:
C     Last modified:
C
      LOGICAL FUNCTION IXSPRP(XSID,BASEL)
C
C-----Arguments:
      INTEGER FCARD
      REAL BASEL
      CHARACTER*16 XSID(1)
C
C-----Argument definitions:
C     XSNIT  - unit number for approach section geometry file
C     XSID   - character identifier for approach section
C
C-----Module data:
      INCLUDE 'APPROC.INC'
C
C-----Variables:
      INTEGER FTEMP(2),FILST,INTYP,NID,TBUNT,FLGS(1),I
      CHARACTER*1 DMNAM
C

      IXSPRP=.FALSE.
      OPEN(UNIT=XSNIT,FILE=XSNAM,STATUS='OLD')
      FTEMP(1)=13
      FTEMP(2)=-9
      INTYP=2
      NID=1
      FLGS(1)=1
      FILST=-3
      FCARD=XSNIT
      DATUM=BASEL
      TBUNT=-1
      I=0
      DMNAM = ' ' 
      CALL HYDIE(FCARD,TBUNT,FTEMP,DMNAM,DMNAM,INTYP,NID,XSID
     #           ,FLGS,I,FILST)
      SIA=I
      IF(FLGS(1).EQ.1.AND.APPID.EQ.XSID(1)) THEN
        DO 10 I=1,NQW
          W1(I)=W1(I)-BASEL
 10     CONTINUE
        IXSPRP=.TRUE.
      ELSE IF(FLGS(1).EQ.3) THEN
        FLGS(1)=-31
        BASEL=-31
        CALL EROCUL (FLGS(1),I)
      ELSE
        FLGS(1)=-9
        BASEL=-9
        CALL EROCUL (FLGS(1),I)
      ENDIF
      CLOSE(UNIT=FTEMP(1))
      CLOSE(UNIT=FCARD)
C
      RETURN
      END
C=====IXSPRP eof==============================================================
C
C=====APPDMP bof==============================================================
C
C-----Purpose:
C       dumps approc.inc commons to ascii file for test and review
C     Programmed by: JM Fulford
C     Date:
C     Modified by:
C     Last modified:
C
      SUBROUTINE APPDMP(OTUNIT,PNO,HTITLE)
C
C-----Arguments:
      INTEGER OTUNIT,PNO
      CHARACTER*80 HTITLE
C
C-----Module data:
      INCLUDE 'APPROC.INC'
      INCLUDE 'CULFLW.INC'
C
C-----Parameters:
      INTEGER NPAGE
      PARAMETER (NPAGE=60)
C
C-----Local variables:
      INTEGER IPAGE,ISTART
C
C-----Formats:
100   FORMAT(A80,/,26X,'APPROACH SECTION PROPERTIES - ID: ',A16,/)
102   FORMAT (8X,'Water',32X,'Top',15X,' Critical',/,7X,'Surface',6X,
     #      'Area',6X,'Conveyance',4X,'width',4X,'Alpha',5X,'discharge'
     #   ,/,7X,'el.(ft)*',4X,'(sq.ft)',6X,'(cfs)',8X,'(ft)',16X,'(cfs)')
101   FORMAT(7X,F7.2,2X,F10.1,2X,F12.1,2X,F8.1,4X,F4.2,2X,F12.1)
105   FORMAT(' ')
106   FORMAT(/,7X,'*elevation referenced to common vertical datum')
202   FORMAT (8X,'Water',32X,'Top',15X,' Critical',/,7X,'Surface',6X,
     #      'Area',6X,'Conveyance',4X,'width',4X,'Alpha',5X,'discharge'
     #   ,/,7X,'el.( m)*',4X,'(sq. m)',6X,'(cms)',8X,'(m)',17X,'(cms)')
201   FORMAT(6X,F8.3,1X,F11.2,1X,F13.2,1X,F9.2,3X,F5.3,1X,F13.2)
C
      PNO=PNO+1
      ISTART=1
      IPAGE=NPAGE-7
      IF(IPAGE.GT.NQW)IPAGE=NQW
 15   CONTINUE
      CALL VERSON(OTUNIT,PNO)
      WRITE(OTUNIT,100)HTITLE,APPID
      IF(SIA.EQ.0.OR.SIA.EQ.3)THEN
      WRITE(OTUNIT,102)
      DO 10 I=ISTART,IPAGE
        WRITE(OTUNIT,101)(W1(I)+BASEL),A1(I),K1(I),B1(I),APH1(I),QC(I)
 10   CONTINUE
      ELSE
      WRITE(OTUNIT,202)
      DO 11 I=ISTART,IPAGE
        WRITE(OTUNIT,201) ft_to_m(W1(I)+BASEL), ft2_to_m2(A1(I)),
     #    ft3_to_m3(K1(I)),ft_to_m(B1(I)),
     #    APH1(I),ft3_to_m3(QC(I))
 11   CONTINUE
      ENDIF
      WRITE(OTUNIT,106)
      IF(IPAGE.LT.NQW) THEN
        ISTART=IPAGE+1
        IPAGE=(NPAGE-7)+ISTART-1
        IF(IPAGE.GT.NQW) IPAGE=NQW
        PNO=PNO+1
      ELSE
        IPAGE=0
      ENDIF
      IF(IPAGE.GT.0) GO TO 15
C
      RETURN
      END
C
C=====APPDMP eof==============================================================
