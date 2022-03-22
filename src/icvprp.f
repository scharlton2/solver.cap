C=====ICVPRP bof=============================================================
C
C-----Purpose:
C       initialize culvert properties table
C     Programmed by: JM Fulford
C     Date:
C     Modified by:
C     Last modified:
C
      LOGICAL FUNCTION ICVPRP(CULID,BASEL)
C
C      IMPLICIT NONE
C
C-----Arguments:
      CHARACTER*16 CULID
      REAL BASEL
C
C-----Argument definitions:
C     CULID - integer identifier for culvert
C
C-----Module data:
      INCLUDE 'CULVRT.INC'
C
C-----Local variables:
      INTEGER NCS,ERR,I,WEB,ERR1,TCR
      REAL B,D,N,SCUL(MXCV),GCUL(MXCV)
C-----Local definitions:
C     CULNIT - unit number to open wspro file containing culvert data on
C     TC     - culvert code type; 1-circular, 2-box, 3-pipe arch, 4-nonstand.
C     NCV    - no. of coordinates describing nonstandard culvert section
C     B      - max. span of culvert, or width for box culvert
C     D      - max. rise of culvert, height for box culvert or diameter for
C              circular culvert
C     WEB    - no. of webs in culvert
C     N      - culvert roughness, Manning's n
C     SCUL   - x coordinate for non-standard culvert section
C     GCUL   - y coordinate for non-standard culvert section
C
C-----Externals:
      INTEGER IXMIN,IXMAX
      EXTERNAL IXMIN,IXMAX
C
      ICVPRP=.FALSE.
      OPEN(UNIT=CULNIT,FILE=CULNAM,STATUS='OLD')
      ERR=0
      NCS=MXCV
C     read data from wspro culvert file
      CALL RCVGEO(CULID,BASEL,D,B,WEB,N,SCUL,GCUL,NCS,ERR)
      CWIDTH=B
      IF(ERR.EQ.1) THEN
        ERR1=-8
        CALL EROCUL(ERR1,I)
      ENDIF
      IF(ERR.NE.0) GO TO 999
      TCR=INT(TC/100)
      NRUFF=N

      CALL CALCCULVPROP(TCR,D,B,WEB,NCS,SCUL,GCUL)

      TC=INT(TC/10)
C
      ICVPRP=.TRUE.
999   CONTINUE
      CLOSE (UNIT=CULNIT)
C
      RETURN
      END
C=====ICVPRP eof=======================================================
C
C=====ICVGEO bof=======================================================
C
C-----Purpose:
C       open wspro file containing culvert geometry and load the unit
C       no. on which the file is openned in culvrt`.
C     Programmed by: JM Fulford
C     Date:
C     Modified by: JM Fulford
C     Last modified: Dec 28, 2011, changed to allow command line input of file names
C       section ids and file header text and the older DOS interactive user inputs
C       expanded the string size allowed (see culvrt.inc) for CULNAM to 132 chars.
C
C     Minor modifications by gfkoltun for 2021 version
C
      LOGICAL FUNCTION ICVGEO()
C
C      IMPLICIT NONE
C
C-----Arguments:
C
C-----Module data:
      INCLUDE 'CULVRT.INC'
C
C-----Argument definitions:
C     CULNIT - unit on which wspro culvert geometry file is openned.
C
C-----local variables
      INTEGER ERR
C
      ICVGEO=.FALSE.
      ERR=1
      CULNIT=10
c     code modified may 26, 2011 to allow all file names and ids for culvert and approach
c     section to be input on command line. Does not trap errors.  Added GETARG calls.
      CALL GETARG(2,CULNAM)
      IF (LEN_TRIM(CULNAM).GT.0) THEN
        OPEN(UNIT=CULNIT,FILE=CULNAM,STATUS='OLD',ERR=300)
        ERR=0
        ICVGEO=.TRUE.
      ELSE
        WRITE(*,*)'Enter file name containing culvert geometry:'
 200    CONTINUE
        READ(*,'(A)') CULNAM     
        IF(CULNAM(1:1).EQ.'#') GOTO 300
        WRITE(*,*) 'file requested is '//TRIM(CULNAM)
        OPEN(UNIT=CULNIT,FILE=CULNAM,STATUS='OLD',ERR=100)
        ERR=0
        ICVGEO=.TRUE.
 100    CONTINUE
        IF(ERR.EQ.1) THEN
          WRITE(*,*) TRIM(CULNAM)//' not found: enter filename or # to q
     #uit'
          CLOSE (UNIT=CULNIT)
          GO TO 200
        ENDIF
      ENDIF
 300  CONTINUE
      CLOSE (UNIT=CULNIT)
C
      RETURN
      END
C
C=====ICVGEO=================================================================
C
C=====RCVGEO==========================================================
C
C-----Purpose:
C       read wspro data file containing culvert data records
C     Programmed by: JM Fulford
C     Date:
C     Modified by: gfkoltun for 2021 version
C     Last modified:
C
      SUBROUTINE RCVGEO
     I                 (CID,
     O                  DATUM,DR,B,WEB,N,SCUL,GCUL,NCS,ERR)
C
C      IMPLICIT NONE
C
C-----Arguments:
      INTEGER WEB,NCS,TCR,ERR
      REAL DR,B,N,SCUL(NCS),GCUL(NCS),DATUM
      CHARACTER*16 CID
C
C-----Argument definitions:
C     CULNIT - file unit number to read wspro data from
C     CID  - culvert id to fetch
C     TC     - culvert code type; 1-circular, 2-box, 3-pipe arch, 4-nonstand.
C     B      - max. span of culvert, or width for box culvert
C     D      - max. rise of culvert, height for box culvert or diameter for
C              circular culvert
C     WEB    - no. of webs in culvert
C     NCV    - no. of coordinates describing nonstandard culvert section
C     N      - culvert roughness, Manning's n
C     SCUL   - x coordinate for non-standard culvert section
C     GCUL   - y coordinate for non-standard culvert section
C
C-----Module data:
      INCLUDE 'CULVRT.INC'
      INCLUDE 'CULFLW.INC'
C
C-----Externals:
      REAL WINGWALL, QC46,KBEV,KRND,KPRJCT
      EXTERNAL WINGWALL, QC46,KBEV,KRND,KPRJCT
C
C-----Local Variables:
      INTEGER FLG,NCHAR,MAX,I,CQFG,CSFG,IO,NCNT,CXFG,ERR1,ERR2,NC5
      INTEGER READY,ERRIO,ITOP,IBOT,PC
      REAL VAR(24),XCTR,BEVD,RNDD,LPRJCT
      CHARACTER*80 CBUF
      CHARACTER*16 BLKOUT,ID
      CHARACTER*3 CODE
C
C-----Formats
 100  FORMAT(A3,A77)
C
      ERRIO=9
      DO 20, I=1,3
        ARAD(I)=0.0
 20   CONTINUE
      BLKOUT='                '
      CXFG=0
      CQFG=0
      CSFG=0
      FLG=0
      ERR=1
      BASEL=0.0
      NHP=-1
      RND=0.0
      KWING=1.0
      BEVD=0.0
      KWR=1.0
      KPROJ=1.0
      KWING=1.0
      TFLW=65
      HFLW=100000.
      VRTID=BLKOUT
      CULID=BLKOUT
      READY=0
      SI=0
      INLET=-1
      C46=0.
      LPRJCT=0.0
      NH5 = -1
 15   CONTINUE
      READ(CULNIT,100,END=999) CODE,CBUF(1:77)
      IF(CODE.EQ.'CV ') THEN
        FLG=0
        ID=BLKOUT
        ID(1:5)=CBUF(3:7)
        IF(CID(1:5).EQ.ID(1:5)) THEN
        FLG=1
        MAX=15
        NCHAR=70
        DO 4 I=1,MAX
          VAR(I)=0.0
 4      CONTINUE
        CALL PCHRR(CBUF(8:77),NCHAR,MAX,NCNT,VAR)
        CLSRD=VAR(1)
        XCTR=VAR(2)
        CLEN=VAR(3)
        BASEL=VAR(4)
C        USINV=VAR(5)
        ZDROP=VAR(5)-VAR(4)
        WEB=INT(VAR(6))-1
        IF(WEB.LT.0)THEN
          ERR1=-101
          CALL EROCUL (ERR1,I)
          WEB=1
        ENDIF
        ERR=0
        VRTID(1:5)=CID(1:5)
        CULID(1:5)=CID(1:5)
        ENDIF
      ELSE IF (CODE.EQ.'XS'.OR.CODE.EQ.'XT') THEN
        FLG=0
      ELSE IF (CODE.EQ.'SI')THEN
        NCHAR=76
        MAX=1
        VAR(1)=0.0
        CALL PCHRR(CBUF(2:77),NCHAR,MAX,NCNT,VAR)
        SI=INT(VAR(1))
      ENDIF
      IF(FLG.EQ.0) GO TO 15
C
      WRITE(ERRIO,100)CODE,CBUF(1:77)
      IF(CODE.EQ.'*ID') THEN
        VRTID(1:16)=CBUF(3:18)
        CULID(1:16)=CBUF(3:18)
        CID(1:16)=CBUF(3:18)
      ELSE
C     reads CG, *C1, *C5, *CS, *CX, *CQ, *CN, *CC, *C3 records
        NCHAR=76
        MAX=24
        DO 14 I=1,MAX
          VAR(I)=0.0
 14     CONTINUE
        CALL PCHRR(CBUF(2:77),NCHAR,MAX,NCNT,VAR)
        IF(CODE.EQ.'CG ')THEN
          IF(NCNT.GE.1)THEN
            TC=INT(VAR(1))
            PC = (TC - 100*(TC/100))/10
C           TCR = shape code: 1=box; 2=circular or ellip; 3=pipe-arch; 4=nonstd
            TCR=TC/100
            IF(TCR.GE.1.AND.TCR.LT.4) THEN
C             shape code 1, 2, or 3: box, circular, elliptical or pipe arch shapes 
C           If there are 3 or more values and shape is box or pipe-arch
              IF(NCNT.GE.3.AND.TCR.EQ.1.OR.NCNT.GE.3.AND.TCR.EQ.3) THEN
                READY=READY+1
C           If there are two or more values and shape is circular
              ELSE IF(NCNT.GE.2.AND.TCR.EQ.2)THEN
                READY=READY+1
C               If rise>0 and span<0.0001 set span = rise
                IF(VAR(2).GT.0.0.AND.VAR(3).LE.0.0001) THEN
                  VAR(3)=VAR(2)
C               If span>0 and rise<=0.0001 set rise=span
                ELSE IF (VAR(3).GT.0.0.AND.VAR(2).LE.0.0001) THEN
                  VAR(2)=VAR(3)
                ENDIF
              ELSE
                ERR=-113
                CALL EROCUL(ERR,I)
              ENDIF
            ENDIF
            D=VAR(2)
            DR=D
            B=VAR(3)
            ARAD(1)=VAR(4)
            ARAD(2)=VAR(5)
            ARAD(3)=VAR(6)
          ENDIF
        ELSE IF(CODE.EQ.'*C1')THEN
C         nhp is initialized to -1. if *C1 exists, nhp >=0
C          WRITE(*,*)'NCNT AT C1',NCNT
          IF(NCNT.GT.1)THEN
            NHP=NCNT/2
            IF(NHP.GT.4) NHP=4
            ERR1=0
            ERR2=0
            DO 5 I=1,NHP
              HP(I)=VAR(I*2)
              CP(I)=VAR(I*2-1)
              IF(CP(I).GT.0.98.OR.CP(I).LT.0.65) ERR1=-11
              IF(I.GT.1) THEN
                IF(HP(I).LT.HP(I-1)) ERR2=-12
              ENDIF
  5         CONTINUE
            IF(ERR1.EQ.-11)THEN
              CALL EROCUL (ERR1,I)
              ERR=ERR1
            ENDIF
            IF(ERR2.EQ.-12)THEN
              CALL EROCUL (ERR2,I)
              ERR=ERR2
            ENDIF
          ELSE IF(NCNT.GT.0) THEN
            NHP=0
            CB12=VAR(1)
            IF(CB12.GT.0.98.OR.CB12.LT.0.85) THEN
              ERR=-13
              CALL EROCUL (ERR,I)
            ENDIF
          ENDIF
C          WRITE(*,*)'NHP',NHP
C          READ(*,*)IIXX
        ELSE IF(CODE.EQ.'*C5')THEN
          IF(MOD(NCNT,2).EQ.0)THEN
            NC5=NCNT/2
            C46=0
          ELSE
            NC5=(NCNT-1)/2
            C46=VAR(1)
            IF(C46.GT.0.98.OR.C46.LT.0.65)THEN
              ERR=-16
              CALL EROCUL(ERR,I)
            ENDIF
          ENDIF
C         C46IN was added so that output of user entered coeffs will
C         be correct.
          C46IN=C46
          ERR1=0
          ERR2=0
          IF(NC5.GT.4) NC5=4
          IF(NC5.GE.1) THEN
            DO 6 I=1,NC5
            H5(I)=VAR(I*2+1)
            C5(I)=VAR(I*2)
            IF(C5(I).GT.0.75.OR.C5(I).LT.0.39) ERR1=-14
              IF(I.GT.1) THEN
                IF(H5(I).LT.H5(I-1)) ERR2=-15
              ENDIF
  6         CONTINUE
            IF(ERR1.EQ.-14) THEN
              CALL EROCUL (ERR1,I)
              ERR=ERR1
            ENDIF
            IF(ERR2.EQ.-15) THEN
              CALL EROCUL (ERR2,I)
              ERR=ERR2
            ENDIF
          ELSE
            ERR=-14
            CALL EROCUL(ERR,I)
          ENDIF
          NH5=NC5
        ELSE IF(CODE.EQ.'*CX')THEN
          IF(CXFG.EQ.0) THEN
            NTW=0
            IF(NCNT.GE.1) READY=READY+100
          ENDIF
          CXFG=1
          IO=NTW
          NTW=NTW+NCNT
          IF(NTW.GT.MAXTW)THEN
            ERR=-6
            CALL EROCUL (ERR,MAXTW)
          ELSE
            DO 7 I=1,NCNT
              H4TW(I+IO)=VAR(I)-BASEL
  7         CONTINUE
          ENDIF
        ELSE IF(CODE.EQ.'*CQ')THEN
          IF(CQFG.EQ.0) THEN
            NQ=0
            IF(NCNT.GE.1) READY=READY+1000
          ENDIF
          CQFG=1
          IO=NQ
          NQ=NQ+NCNT
          IF(NQ.GT.MAXQ)THEN
            ERR=-7
            CALL EROCUL (ERR,MAXQ)
          ELSE
            DO 8 I=1,NCNT
              Q(I+IO)=VAR(I)
  8         CONTINUE
          ENDIF
        ELSE IF(CODE.EQ.'*CS')THEN
          IF(CSFG.EQ.0) THEN
            NCS=0
            IF(NCNT.GE.2)READY=READY+1
          ENDIF
          CSFG=1
          IO=NCS
          NCS=NCS+NCNT/2
          IF(NCS.GT.MXCV)THEN
            ERR=-5
            CALL EROCUL (ERR,MXCV)
          ELSE
            DO 9 I=1,NCNT/2
              SCUL(IO+I)=VAR(2*I-1)
              GCUL(IO+I)=VAR(2*I)
  9         CONTINUE
          ENDIF
          TCR=4
          TC=400
        ELSE IF(CODE.EQ.'*CN') THEN
          IF(NCNT.GE.1) READY=READY+10
          N=VAR(1)
        ELSE IF(CODE.EQ.'*C3') THEN
C          WRITE(*,*)'VAR(1) - VAR(4)',VAR(1),VAR(2),VAR(3),VAR(4)
C          READ(*,*)IXXXK
C         Note: NHP is less than zero only if a *C1 record was not already read (if *C1 record was read,
C               ignore all parameters except the inlet type)    
           IF (NHP.LT.0) THEN
C         Note: INLET is negative if not already set
              IF(INLET.GT.0)THEN
                ERR1=-110
                CALL EROCUL(ERR1,I)
                RNDD=0.0
                BEVD=0.0
                LPRJCT=0.0
              ENDIF
              IF(VAR(3).GT.0) THEN
                THETA=VAR(3)
                KWING=WINGWALL(VAR(3))
              ENDIF
              IF(VAR(1).GT.1.0)THEN
                KWR=VAR(1)
              ELSE IF(VAR(2).GT.1.0) THEN
                KWR=VAR(2)
              ENDIF
              IF(VAR(5).GT.0) KPROJ=VAR(5)
              IF(KWING.LT.1.0)THEN
                ERR1=-104
                CALL EROCUL (ERR1,I)
                KWING=1.0
              ENDIF
              IF(VAR(1).LT.1.0)THEN
C           rounding coefficient error
                ERR1=-102
                CALL EROCUL (ERR1,I)
              ENDIF
              IF(VAR(2).LT.1.0)THEN
C           beveling coefficient error
                ERR1=-103
                CALL EROCUL (ERR1,I)
              ENDIF
              IF(KPROJ.GT.1.0)THEN
                ERR1=-106
                CALL EROCUL (ERR1,I)
                KPROJ=1.0
              ENDIF
          ENDIF
          INLET=INT(VAR(4))
          IF(INLET.LT.1.0.OR.INLET.GT.4.0)THEN
              ERR1=-105
              CALL EROCUL (ERR1,I)
              INLET=1
          ENDIF
        ELSE IF(CODE.EQ.'*CC') THEN
C         Note that inlet is negative if not already set
            IF(INLET.LE.0) THEN
            THETA=VAR(3)
            RNDD=VAR(1)
            BEVD=VAR(2)
            IF(VAR(3).GT.0) KWING=WINGWALL(VAR(3))
            INLET=INT(VAR(4))
            IF(VAR(5).GT.0) LPRJCT=VAR(5)
            IF(INLET.LT.1.OR.INLET.GT.4) THEN
                ERR1=-105
                CALL EROCUL (ERR1,I)
                INLET=1
            ENDIF 
            INLET=-INLET
            ELSE
C           ignore the *CC record if *C3 is entered for culvert
            ERR1=-110
            CALL EROCUL(ERR1,I)
            ENDIF
        ELSE IF(CODE.EQ.'*CF') THEN
          IF(NCNT.GE.1) READY=READY+100000
          TFLW=INT(VAR(1))
          IF (NCNT.GE.2) THEN
            HFLW=VAR(2)
          ENDIF
        ENDIF
      ENDIF
      GO TO 15
C
999   CONTINUE
C     If the material code was set to 2 set the inlet code to mitered (even if set otherwise
C     on a *CC or *C3 record) - DECIDED AGAINST OVERIDE
C      IF (PC.EQ.2) INLET=-2
C     If inlet type > 0 and there is a *C1 card or C46 has been specified then error 
C     otherwise set inlet to non-neg value
      IF(INLET.GT.0.AND.NHP.GT.-1.OR.INLET.GT.0.AND.C46.GT.0.) THEN
C        both *C1 and *C3 or *C5 and *C3 are entered, 
C        *C3 record is ignored except for INLET code warning message - NOT IGNORING *C3
C         ERR1 = -118
C         CALL EROCUL(ERR1,I)
      ELSE 
         INLET = IABS(INLET)
      ENDIF  
      IF(INLET.LE.0) INLET=1
      
C     Calculate C3 parameters based on span for box culverts and rise for all others
      IF(TCR.EQ.1) THEN
        CALL CALCC3FROMCC(RNDD,BEVD,B,LPRJCT)  
      ELSE
C       The code in the if statement added to compute the rise for a
C       nonstandard culvert. The rise is assumed to be the difference 
C       between the min and max Y coordinate, converted to in or cm
        IF (TCR.EQ.4) THEN
          ITOP=IXMAX(GCUL,NCS)
          IBOT=IXMIN(GCUL,NCS)
          D=GCUL(ITOP)-GCUL(IBOT)
          IF ((SI.EQ.0).OR.(SI.EQ.2)) THEN
            D=D*12.
          ELSE
            D=D*100.
          ENDIF
        ENDIF
        CALL CALCC3FROMCC(RNDD,BEVD,D,LPRJCT) 
      ENDIF

      IF(SI.EQ.1.OR.SI.EQ.3)THEN
C       convert from m/s to ft/s units
        CALL CONVTOFTUNIT(NCS,SCUL,GCUL,B)
      ELSE
C       convert from inch to ft
        B=B/12.0
        D=D/12.0
      ENDIF

      DR=D  
C     Compute default C46 if no C46 is read from input data
      IF(C46.LE.0) C46 = QC46(TC,BEVD,KWR,RND,THETA,INLET,KPROJ)
C
      DATUM=BASEL
      IF(HFLW.EQ.-999.) HFLW=1.5*DR+ZDROP+BASEL
      IF((CSFG.GT.0.AND.NH5.LE.0).OR.(CSFG.GT.0.AND.NHP.LE.0)) THEN
        ERR1= -108
        CALL EROCUL (ERR1, I)
      ENDIF
      IF(READY.EQ.0)THEN
        ERR1=-8
        CALL EROCUL(ERR1,I)
        ERR=-17
      ELSE
        READY=READY+10000
        IF(READY.NE.111111.AND.READY.NE.011111) THEN
C        .AND.READY.NE.111112
C     #     .AND.READY.NE.011112) THEN
C         culvert section not described sufficiently
          ERR=-17
Cwrite          WRITE(*,*)'READY IN RCVGEO',READY,ERR
          CALL EROCUL (ERR,I)
        ENDIF
        IF(READY.LE.100000) THEN
C         *CF record omitted
          ERR1=-109
          CALL EROCUL(ERR1,I)
        ELSE
          READY=READY-100000
        ENDIF
C        IF(READY.LT.10000) THEN
C         *C5 record omitted section commented out 12.96 for default C5 comps
C          ERR1=-110
C          CALL EROCUL (ERR1,I)
C        ELSE
         IF(READY.GE.10000) THEN
           READY=READY-10000
         ENDIF
        IF(READY.LT.1000)THEN
C         *CQ record omitted
          ERR1=-117
          CALL EROCUL (ERR1,I)
        ELSE
          READY=READY-1000
        ENDIF
        IF(READY.LT.100)THEN
C         *CX omitted
          ERR1=-116
          CALL EROCUL (ERR1,I)
        ELSE
          READY=READY-100
        ENDIF
        IF(READY.LT.10)THEN
C         *CN omitted
          ERR1=-115
          CALL EROCUL (ERR1,I)
        ELSE
          READY=READY-10
        ENDIF
        IF(READY.EQ.2)THEN
C         CG and *CS records in file
          ERR1= -112
          CALL EROCUL (ERR1,I)
        ENDIF
      ENDIF
C
      RETURN
        END
C
C=====RCVGEO eof=================================================================
C=====QC46 bof=================================================================
C
C-----Purpose:
C      computes default C46 discharge coefficients according to the TWRI
C      chapter A3, "Measurement of Peak Discharge at Culverts by Indirect
C      Methods", pg 42 and 43 using table 5.
C     Programmed by: JM Fulford
C     Modified by gfkoltun for 2021 version
C
      REAL FUNCTION QC46(TCR,BEVD,KWR,RND,THETA,INLET,KPROJ)
c

C-----Arguments:
      INTEGER INLET,TCR,TC,MC
      REAL BEVD,KWR,THETA,KPROJ
C
C-----Local variables:
C
      QC46=1.0
C     Note that TC equals:
C     1 for box, 2 for circular/elliptical, 3 for pipe arch, and 4 for other      
      TC=TCR/100
C     Change from QC46 to C46 below to allow coefficient to be computed 
C     and potentially adjusted if pipe is projecting
      IF (INLET.EQ.2)THEN
C       mitered pipe
        C46=0.74
      ELSE IF(INLET.EQ.4)THEN
C       flared pipe ends
        QC46=0.90
      ELSE IF(THETA.LT.75.0.OR.TC.GT.1)THEN
C       Eqn below approximately reproduces table 5 but decreases when RND>0.14 !!!
        C46=0.84 + 2.125*RND - 8.035*RND*RND
C       Set lower limit of 0.87 for C46 when theta ge 30 and culvert is circular
        IF(THETA.GE.30.AND.C46.LT.0.87.AND.TC.EQ.1) C46=0.87
      ELSE IF(THETA.GE.75.0.AND.TC.EQ.1)THEN
        C46=(1.47 - 0.008*THETA)*KWR
      ENDIF
C     Compute the material code to facilitate test for concrete pipes with beveling
      MC=(TCR-(100*(TCR/100)))/10
      IF (INLET.NE.4) THEN
        IF ((MC.EQ.1).AND.(BEVD>0.)) THEN
C         Don't apply projection correction to beveled concrete pipes
          QC46=C46
        ELSE
          QC46=KPROJ*C46
        ENDIF
      ENDIF
C
      RETURN
      END
C
C=====QC46 eof=================================================================
C
C=====KRND bof=================================================================
C
C-----Purpose:
C       interpolates rounding adjustment coefficients
C     Programmed by: JM Fulford
C
      REAL FUNCTION KRND(RND)
C
C-----Arguments:
      REAL RND
C
C-----Local variables
C
      IF(RND.LE.0.115) THEN
        KRND=1.0005 + 1.9662*RND + 4.5275*RND*RND - 61.792*RND*RND*RND
      ELSE IF (RND.LE.0.133)THEN
        KRND=1.192 + 0.4444*(RND - 0.115)
      ELSE
        KRND=1.2
      ENDIF
C
      RETURN
      END
C
C=====KRND eof=============================================================
C
C=====KBEV bof=============================================================
C
C-----Purpose:
C        compute coefficient adjustments for beveling of box or pipe
C     Programmed by: JM Fulford
C     Date: 12.96
C
      REAL FUNCTION KBEV(RND,THETA)
C
C-----Arguments:
      REAL RND,THETA
C
C-----Local variables:
      REAL Y,Y2,Y3
C     Following code implements (sort of) fig. 22
      IF(THETA.LE.0.0) THEN
        KBEV=1.0
      ELSE IF(THETA.LE.30.) THEN
        Y=0.99973 + 1.4707*RND - 4.77*RND*RND - 11.794*RND*RND*RND
        KBEV=1.0 + THETA*(Y - 1.0)/30.0
      ELSE IF (THETA.LE.45.0) THEN
        Y=0.99973 + 1.4707*RND - 1.77*RND*RND - 11.794*RND*RND*RND
        Y2=0.99897 + 3.6457*RND - 25.459*RND*RND + 40.508*RND*RND*RND
        KBEV = Y + (THETA - 30.0)*(Y2-Y)/15.0
      ELSE IF(THETA.LE.60)THEN
        Y2=0.998997 + 3.6457*RND - 25.459*RND*RND + 40.508*RND*RND*RND
        Y3=1.0 + 4.8351*RND - 18.307*RND*RND -19.827*RND*RND*RND
        KBEV=Y2 + (THETA-45.0)*(Y3-Y2)/15.0
C     Use values for theta=60 when theta>60
      ELSE IF (THETA.GT.60) THEN
        KBEV=1.0+4.8351*RND - 18.307*RND*RND - 19.827*RND*RND*RND
      ENDIF
C
      RETURN
      END
C
C=====KBEV eof=================================================================
C
C=====KPRJCT bof===============================================================
C
C-----Purpose:
C       computes the coefficient adjustment for projecting pipes
C-----Programmed by: JM Fulford
C     Modified by gfkoltun for 2021 version
C
      REAL FUNCTION KPRJCT(LPRJCT)
C
C-----Arguments:
      REAL LPRJCT
C
C-----Local variables:
C
C     KPRJCT reaches its minimum value at LPRJCT=1 so set larger values to 1
      IF(LPRJCT.GT.1.0) LPRJCT=1.0
      
      IF(LPRJCT.LT.0.1)THEN
        KPRJCT = 0.99909 - 0.78182*LPRJCT
      ELSE IF(LPRJCT.GE.0.1)THEN
        KPRJCT = 0.92246 - 0.024299*LPRJCT
      ENDIF
C
      RETURN
      END
C
C=====KPRJCT eof===============================================================
C
C=====CLDEPTH bof==========================================================
C
C-----Purpose:
C      return maximum vertical dimension of culvert
C     Programmed by: JM Fulford
C     Date:
C     Modified by:
C     Last modified:
C
      REAL FUNCTION CLDEPTH(CLID)
C
C-----Arguments:
      CHARACTER*16 CLID
C
C-----Module data:
      INCLUDE 'CULVRT.INC'
C
C-----Externals:
      LOGICAL IDMATCH
      EXTERNAL IDMATCH
C
      IF(.NOT.IDMATCH(CLID,VRTID))THEN
Cwrite         WRITE(*,*)'ERROR IN CLDEPTH'
      ENDIF
      CLDEPTH=CULD(TBSIZE)
C
      RETURN
      END
C
C=====CLDEPTH eof==========================================================
C
C=====IDMATCH bof==========================================================
C
C-----Purpose:
C       checks for match between asked for id and id in common
C     Programmed by: JM Fulford
C     Date:
C     Modified by:
C     Last modified:
C
      LOGICAL FUNCTION IDMATCH(ID1,ID2)
C
C-----Arguments:
      CHARACTER*16 ID1,ID2
C
C-----Local variables:
      INTEGER IFLG,I
C
      IDMATCH=.TRUE.
      IFLG=0
      I=0
10    CONTINUE
      I=I+1
      IF(ID1(I:I).NE.' ') THEN
        IF(ID1(I:I).NE.ID2(I:I)) THEN
          IFLG=1
          IDMATCH=.FALSE.
        ENDIF
      ELSE
        IFLG=2
      ENDIF
      IF(IFLG.NE.0.AND.I.LT.16) GO TO 10
C
      RETURN
      END
C
C=====IDMATCH eof==========================================================
C
C=====NONSTD  bof==========================================================
C
C-----Purpose:
C       gets the area, conveyance, top width and wetted perimeter for
C       enclosed conduit sections from the HYDIE routines
C     Programmed by: JM Fulford
C     Date:
C     Modified by:
C     Last modified:
C
      SUBROUTINE NONSTD
     I                 (NCS,SCUL,GCUL,DC,N,AREA,KONVEY,TW,WP)
C
C-----Arguments:
      INTEGER NCS
      REAL SCUL(NCS),GCUL(NCS),DC,N,AREA,KONVEY,TW,WP
C
C-----Argument definitions:
C     SCUL    - station point
C     GCUL    - ground elevation
C     DC      - depth at which to compute hydraulic properties
C     N       - Mannning's roughness for the section
C     AREA    - area at DC
C     KONVEY  - conveyance at DC
C     TW      - top width at DC
C     WP      - wetted perimeter at DC
C
C-----Local variables:
      INTEGER KXND,NRUF,I,NSA
      REAL POUT(17),SUBPRP(7,1),RUF(3,1),SIN(3,1),XSA(1)
C
      KXND=1
      NRUF=1
      RUF(1,1)=1
      RUF(2,1)=N
      DO 10 I=1,3
        SIN(I,1)=0.0
 10   CONTINUE
      NSA=1
      XSA=0.0
      CALL PROPER(SCUL,GCUL,NCS,NSA,XSA,DC,NRUF,RUF,SIN,POUT,
     #            SUBPRP,KXND)
      AREA=POUT(3)
      KONVEY=POUT(1)
      TW=POUT(2)
      WP=POUT(6)
C
      RETURN
      END
C
C=====NONSTD  eof==========================================================
C
C=====CULOUT bof==============================================================
C
C-----Purpose:
C       puts values into approac.inc from pout array. Called by hydie
C     Programmed by: JM Fulford
C     Date:
C     Modified by:
C     Last modified:
C
      SUBROUTINE CULOUT
     O                 (RD,XSNAME,INKT,NKTS,POUT)
C
C-----Arguments:
      INTEGER NKTS,INKT
      REAL RD,POUT(17)
      CHARACTER*16 XSNAME
C
C-----Module data:
      INCLUDE 'APPROC.INC'
C
      NQW=NKTS
      APPID=XSNAME
Cwrite      WRITE(*,*)'XSNAME',XSNAME
      APSRD=RD
      I=INKT
C      DO 10 I=1,NKTS
        W1(I)=POUT(13)
        A1(I)=POUT(3)
        APH1(I)=POUT(9)
        K1(I)=POUT(1)
        B1(I)=POUT(2)
        WP1(I)=POUT(6)
        QC(I)=0.0
        IF(W1(I).GT.0.0.AND.A1(I).GT.0.0.AND.B1(I).GT.0.0)
     #    QC(I)=A1(I)*SQRT(32.2*A1(I)/B1(I))
C 10   CONTINUE
C
      RETURN
      END
C
C=====CULOUT eof==============================================================
C
C=====EROCUL bof==============================================================
C
C-----Purpose:
C        writes culvert openning and reading errors, all fatal
C     Programmed by: JM Fulford
C     Date:
C     Modified by: gfkoltun for 2021 version
C     Last modified:
C
      SUBROUTINE EROCUL
     I                 (ERRNO,NUMBER)
C
C-----Arguments:
      INTEGER ERRNO,NUMBER
C
C-----Local variables
      INTEGER PRTNO
      CHARACTER*45 ERRMESS(34)
C
C-----PARAMETER
      INTEGER ERRIO
      PARAMETER (ERRIO=-9)
C
C-----Initializations:
      DATA (ERRMESS(I), I=1,15) /
     #' allowable x,y coordinates, -------- EXCEEDED',
     #' allowable tailwater entries ------- EXCEEDED',
     #' allowable discharge entries ------- EXCEEDED',
     #' Culvert section ID NOT FOUND in file -------',
     #' Approach section ID NOT FOUND in file ------',
     #' Approach section located incorrectly - FATAL',
     #' CP coefficients unreasonable --------- FATAL',
     #' Depth ratios for CP values funny ----- FATAL',
     #' CB12 coefficients unreasonable ------- FATAL',
     #' C5 coefficients unreasonable --------- FATAL',
     #' Depth ratios for C5 values funny ----- FATAL',
     #' C46 coefficients unreasonable -------- FATAL',
     #' Culvert data incomplete, check data -- FATAL',
     #' NBBL, no. of barrels <1, default of 1 used  ',
     #' KR <1, default of 1 used                    '/
      DATA (ERRMESS(I), I=16,34) /
     #' KW <1, default of 1 used                    ',
     #' THETA <0 or >90, default of 0 used          ',
     #' INLET code not valid, default of 1 used     ',
     #' KPROJ >1, default of 1 used                 ',
     #' Approach section located too close --WARNING',
     #' No *C5,*C1 for nonstandard culvert --WARNING',
     #' No *CF record, type 6 and 5 flows computed  ',
     #' *C3 record used, *CC record ignored -WARNING',
     #' Approach data incomplete, check data - FATAL',
     #' Conflicting records, CG and *CS exist  FATAL',
     #' Missing data on CG record ------------ FATAL',
     #' No *PD record in approach section file FATAL',
     #' No *CN record, ----------------------- FATAL',
     #' No *CX record, ----------------------- FATAL',
     #' No *CQ record, ----------------------- FATAL',
     #' *C3 record ignored except inlet code-WARNING',
     #' Computed BOTRAD outside calib. range-WARNING',
     #' Computed TOPRAD outside calib. range-WARNING',
     #' Computed CORRAD outside calib. range-WARNING'      
     # /
C
C-----FORMAT
 100  FORMAT(1X,I3,A45)
 101  FORMAT(4X,A45)
C
      IF(ERRNO.GT.-100) THEN
        PRTNO=-1*ERRNO-4
      ELSE
        PRTNO=-1*ERRNO-87
      ENDIF
      IO=ERRIO
      IF(ERRIO.LT.0) THEN
        IO=ABS(ERRIO)
        IF(PRTNO.LE.3)THEN
Cwrite          WRITE(*,100)NUMBER,ERRMESS(PRTNO)
        ELSE IF((PRTNO.LT.32).AND.(PRTNO.NE.5)) THEN
          WRITE(*,101)ERRMESS(PRTNO)
        ENDIF
      ENDIF
      IF(PRTNO.LE.3)THEN
        WRITE(IO,100)NUMBER,ERRMESS(PRTNO)
      ELSE IF(PRTNO.NE.5) THEN
        WRITE(IO,101)ERRMESS(PRTNO)
      ENDIF
C
      RETURN
      END
C
C=====EROCUL Eof==============================================================

C=====CONVTOFTUNIT bof==============================================================
C
C-----Purpose:
C        convert unit from meter/centimeter to ft
C     Programmed by: Keisuke Inoue
C     Date:
C     Modified by:
C     Last modified:
C
      SUBROUTINE CONVTOFTUNIT
     I                 (NCS,SCUL,GCUL,B)
C
      implicit none

C-----Arguments:
      REAL SCUL(NCS),GCUL(NCS)
      INTEGER NCS
      REAL:: B

      real:: cm_to_ft
      real:: m_to_ft

      include 'CULVRT.INC'
      include 'CULFLW.INC'
C
C-----Local variables
      INTEGER NCONVRT

      CLEN  = m_to_ft(CLEN)
      CLSRD = m_to_ft(CLSRD)
      BASEL = m_to_ft(BASEL)
      ZDROP = m_to_ft(ZDROP)

      call m_to_ft_array(NTW,H4TW)
      call m_to_ft_array(NCS,SCUL)
      call m_to_ft_array(NCS,GCUL)
      call m3_to_ft3_array(NQ,Q)

      D = cm_to_ft(D)
      B = cm_to_ft(B)

      NCONVRT=3
      call cm_to_inch_array(NCONVRT,ARAD)
      END
C
C=====CONVTOFTUNIT Eof==============================================================

C=====CALCC3FROMCC bof==============================================================
C
C-----Purpose:
C        Calculate *C3 parameters from *CC parameters
C     Programmed by: Keisuke Inoue
C     Date:
C     Modified by:
C     Last modified:
C
      SUBROUTINE CALCC3FROMCC
     I                 (RNDD,BEVD,D,LPRJCT)
C
C-----Arguments:
      REAL RNDD,BEVD,D,LPRJCT

      include 'CULVRT.INC'

      REAL KBEV,KRND,KPRJCT
      EXTERNAL KBEV,KRND,KPRJCT

      IF(RNDD.GT.0)THEN
        RND=RNDD/D
        KWR=KRND(RND)
      ELSE IF(BEVD.GT.0.0)THEN
        RND=BEVD/D
        IF(RND.GT.0.AND.RND.LT.0.1) THEN
          KWR=KBEV(RND,THETA)
          KWING=1.0
        ELSE
          KWR=1.0
        ENDIF   
      ENDIF
      IF(LPRJCT.GT.0.0)THEN
        LPRJCT=LPRJCT/D
        KPROJ=KPRJCT(LPRJCT)
      ENDIF
      END
C
C=====CALCC3FROMCC Eof==============================================================

C=====CALCCULVPROP bof==============================================================
C
C-----Purpose:
C        Calculate culvert properties (CULD, CULA, etc)
C     Programmed by: Keisuke Inoue
C     Date:
C     Modified by:
C     Last modified:
C
      SUBROUTINE CALCCULVPROP
     I                 (TCR,D,B,WEB,NCS,SCUL,GCUL)

      IMPLICIT NONE
C
C-----Arguments:
      REAL D,B,SCUL(NCS),GCUL(NCS),BEL
      INTEGER TCR,WEB,NCS,XMNWID,XMXWID

      include 'CULVRT.INC'
C
C-----Local variables
      REAL DC,DCEL,AREA,KONVEY,TW,WP
      INTEGER FLG,I,ITOP,IBOT

      INTEGER IXMIN,IXMAX
      EXTERNAL IXMIN,IXMAX

      CULD(1)=0.0
      CULA(1)=0.0
      CULK(1)=0.0
      CULT(1)=0.0
      CULWP(1)=0.0
      IF (TCR.EQ.2) THEN
C       compute properties for circular & elliptical culvert sections

        IF(ABS(D-B).LE. 0.000001) THEN
          DO I=2,TBSIZE
            DC=FLOAT(I-1)*D/(FLOAT(TBSIZE-1))
            CALL ARCIR (D,DC,NRUFF,AREA,KONVEY,TW,WP)
            CULD(I)=DC
            CULA(I)=AREA
            CULK(I)=KONVEY
            CULT(I)=TW
            CULWP(I)=WP
          END DO
        ELSE
          DO I=2,TBSIZE
            DC=FLOAT(I-1)*D/(FLOAT(TBSIZE-1))
            CALL ELLIPSE (D,B,DC,NRUFF,AREA,KONVEY,TW,WP)
            CULD(I)=DC
            CULA(I)=AREA
            CULK(I)=KONVEY
            CULT(I)=TW
            CULWP(I)=WP
          END DO
        ENDIF  
      ELSE IF (TCR.EQ.1) THEN
C       compute properties for rectangular culvert sections

        DO I=1,TBSIZE
          DC=FLOAT(I-1)*D/(FLOAT(TBSIZE-1))
          CALL ARBOX (DC,B,D,NRUFF,WEB,AREA,KONVEY,TW,WP)
          CULD(I)=DC
          CULA(I)=AREA
          CULK(I)=KONVEY
          CULT(I)=TW
          CULWP(I)=WP
        END DO
      ELSE IF (TCR.EQ.3) THEN
C       compute properties for pipe arc sections, these routines are
C       slightly altered versions of the ones used by WSPRO

        I=0
        FLG=0
 20     CONTINUE
        I=I+1
        IF(ARAD(I).LE.0) FLG=1
        IF(FLG.NE.1.AND.I.LT.3) GO TO 20
C       CULPAD is called unless all radii are non-zero
        IF(FLG.EQ.1) CALL CULPAD(TC,D,B,ARAD)
        DO I=2,TBSIZE
          DC=FLOAT(I-1)*D/(FLOAT(TBSIZE-1))
          CALL ARCH (DC,B,D,ARAD,NRUFF,AREA,KONVEY,TW,WP)
          CULD(I)=DC
          CULA(I)=AREA
          CULK(I)=KONVEY
          CULT(I)=TW
          CULWP(I)=WP
        END DO
      ELSE IF (TCR.EQ.4) THEN
C       compute properties for non-standard culvert sections from
C       entered coordinates (*CS records)

        BEL=GCUL(IXMIN(GCUL,NCS))
C       INSERT STUFF FOR CULVERT WIDTH HERE
        XMNWID=IXMIN(SCUL,NCS)
        XMXWID=IXMAX(SCUL,NCS)
        CWIDTH=SCUL(XMXWID)-SCUL(XMNWID)
        DO I=2,TBSIZE
          ITOP=IXMAX(GCUL,NCS)
          IBOT=IXMIN(GCUL,NCS)
          D=GCUL(ITOP)-GCUL(IBOT)
          DC=FLOAT(I-1)*D/(FLOAT(TBSIZE-1))
          DCEL=DC+BEL
          CALL NONSTD (NCS,SCUL,GCUL,DCEL,NRUFF,AREA,KONVEY,TW,WP)
          CULD(I)=DC
          CULA(I)=AREA
          CULK(I)=KONVEY
          CULT(I)=TW
          CULWP(I)=WP
        END DO
      ENDIF
      END
C
C=====CALCCULVPROP Eof==============================================================
