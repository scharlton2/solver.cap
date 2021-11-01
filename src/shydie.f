      SUBROUTINE HYDIE
     I                (FCARD,TBUNT,FTEMP,CDNAM,TBNAM,INTYP,NID,IDLST
     M                 ,FLGS,SI,FILST)
C
C     + + + PURPOSE + + +
C     THIRD DEVELOPEMENT PROGRAM.  USES A WSPRO DATA SET AS THE DATA BASE
C     AND NOT A WDM FILE AS IN PREVIOUS VERSIONS.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER FCARD,TBUNT,INTYP,NID,FLGS(NID),FILST,FTEMP(2),SI
      CHARACTER*16 IDLST(NID)
      CHARACTER*(*) CDNAM,TBNAM
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SI      - units flag input/output 0-fts/fts 1-ms/ms 2-fts/ms 3-ms/fts
C     FCARD   - file unit no. for data file
C     TBUNT   - file unit no. for table output
C     FTEMP(1)   - file unit no. for scratch file
C     FTEMP(2)   - file unit no. for error printing, <0 prints to screen and
C                  file no. and output
C     CDNAM   - data file name of input data file
C     TBNAM   - file name of table output
C     INTYP   - type of input datafile, 2- wspro, 3- xhydrp
C               1- reserved for future
C     NID     - number of cross sections to select
C     IDLST   - (NID) array of section id's to select from datafile
C     FLGS    - (NID) array of branch numbers for IDLST array on input.
C               On return the array contains search and error flags
C               for section id's. 0- found, -1-not found, -2- read error.
C     FILST   - file status flags 0- does entire data file, no files are
C               opened when the subroutine is called.  1-multi branch
C               file, temp file doesn't exist, no files openned.
C               2- temp file doesn't exist, input and output files open.
C               3- temp file exists, all files open, returns this value
C               on exit if successful files.
C               -1 no files opened, does only selected cross sections.
C               (Opening errors only, no read errors)
C
C     + + + PARAMETERS + + +
C     NCORD=no. of coordinate pairs,  NSUBS=no. of subareas,  NSBS=2(nsubs+1)
C     NXSEC=no. of cross sections
C
      INTEGER NXSEC,NCORD,NSUBS,NSBS
      PARAMETER (NXSEC=200, NCORD=150, NSUBS=19, NSBS=40)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER NKTS,I,NCOMP,K,IREC,II,N,ERR,EDF,NRUF,NOUT,MATCH,
     #       NHDR,NCARDS,XSREC(NXSEC),KXND,ERFG,EREC,NSIN,TBTYP(3)
     #       ,FNDXSC,J,ISEC,OPOUT(17),NSA,BRNF,NSECT
     #       ,IHDR,IOVER,ERRIO,ERRNO,IOLD,NSTOP,KTS,NOHP,KK,SL1
C
      REAL S(NCORD),G(NCORD),XSA(NSUBS),POUT(17,1),SIN(3,NSBS),
     #  RATIO,RUF(3,NSBS),SECPRP(3,NXSEC),MAXDEP,X(NXSEC),SRD,
     #     COMP(6,5),MINDEP,SUBPRP(7,NSUBS),DATUM
      CHARACTER*16 SECID,ELID(NXSEC)
      CHARACTER*75 TITLES(3)
C
C     + + + EXTERNALS + + +
      INTEGER MATSTR,IXMAX,IXMIN
      EXTERNAL WSPDA,HYDDA,WSPSEQ,KNOTS,WSPCDS,PROPER,STAGE,HYDOUT,
     #         FNDXSC,OUTTAB,IXMIN,BRNOUT,MATSTR,IXMAX
C
C     + + + INSTRINSICS + + +
      INTRINSIC INT
C
C     + + + INITIALIZATIONS + + +
      DATA IOVER /1000/
C
      ERRIO=FTEMP(2)
      KTS=1
      NCARDS=1
      IREC=1
      NCOMP=0
      ISEC=0
      NHDR=0
      ERFG=0
      TBTYP(1)=0
C
      IF(FILST.LE.2)THEN
      IF(INTYP.EQ.1) THEN
C     * call terminal input subroutine *
        PRINT*,'NOT IMPLEMENTED'
        GO TO 999
      ELSE
        OPEN(UNIT=FTEMP(1),STATUS='SCRATCH',ACCESS='DIRECT',
     #       FORM='FORMATTED',RECL=80)
        IF(FILST.LE.1.AND.FILST.NE.-3)THEN
          OPEN(UNIT=FCARD,FILE=CDNAM,STATUS='OLD',ERR=999)
          IF(FILST.LE.-1)THEN
            OPEN(UNIT=TBUNT,STATUS='SCRATCH')
          ELSE
            OPEN(UNIT=TBUNT,FILE=TBNAM,STATUS='NEW')
          ENDIF
        ENDIF
        IF (INTYP.EQ.2) THEN
C     * open WSPRO file and transfer into temp DA *
        CALL WSPDA (FCARD,FTEMP,NCOMP,COMP,ISEC,SECPRP,ELID,NCARDS,
     #              NHDR,TBTYP,XSREC,NOUT,OPOUT,TITLES,SI,ERFG)
        ELSE IF(INTYP.EQ.3)THEN
C     * open XHYDRP file and transfer into temp DA*
        CALL HYDDA (FCARD,FTEMP,NCARDS,NCOMP,COMP,NHDR,SECPRP,ELID,
     #               TBTYP,XSREC,NOUT,OPOUT,ERFG)
        ISEC=NHDR
        ENDIF
       ENDIF
      ELSE
         CALL DACOMP(FTEMP,
     O                 NCOMP,COMP,ISEC,SECPRP,ELID,NCARDS,NHDR,TBTYP,
     #                 XSREC,NOUT,OPOUT)
      ENDIF
C     *** patch for eliminating *TT record for WSPRO input in slope-area
C     *** program SAC
      NOHP=-1
      IF(FILST.EQ.-2) THEN
        NOHP=ISEC
        ISEC=NHDR
        IF(TBTYP(1).EQ.0) THEN
          FILST=-1
          TBTYP(1)=4
        ELSE IF(TBTYP(1).EQ.4) THEN
          FILST=-1
        ENDIF
      ELSE IF (FILST.EQ.-3) THEN
C     *** patch for eliminating *TT record for WSPRO input in culvert
C     *** program CAP
        IF(TBTYP(1).EQ.0) THEN
          FILST=2
          TBTYP(1)=5
        ELSE IF(TBTYP(1).EQ.5) THEN
          FILST=2
        ENDIF
      ENDIF
C
      IF(ERFG.EQ.1) GO TO 999
      


      IF(NCOMP.EQ.0.AND.ISEC.EQ.0.AND.FILST.LT.0) THEN
C       * no computations to be made *
        II=3
C        OPEN(UNIT=TBUNT,FILE='OUTFL',STATUS='NEW')
        CALL WSPSEQ(FTEMP(1),TBUNT,NCARDS)
      ELSE IF(TBTYP(1).EQ.5.AND.NCOMP.EQ.0)THEN
        FLGS(1)=3
      ELSE
        IF(TBTYP(1).EQ.4)THEN
240       FORMAT (A75)
          DO 5 I=1,3
            WRITE(TBUNT,240)TITLES(I)
 5        CONTINUE
        ENDIF
C 2      CONTINUE
        NSA=-1
        N=0
        NRUF=0
        NSIN=0
      IF(NCOMP.GT.0) THEN
        I=0
        BRNF=I
        IF(FILST.LE.0)THEN
          NSECT=NHDR
        ELSE
          NSECT=NID
        ENDIF
        I=0
        DO 25 K=1,NSECT
          EDF=0
          IF(FILST.GT.0) THEN
            IHDR=FNDXSC(IDLST(K),FTEMP(1),XSREC,NHDR)
            IF(IHDR.GT.0) THEN
              IREC=XSREC(IHDR)
              EREC=XSREC(IHDR+1)-1
              IF(IHDR.EQ.NHDR) EREC=NCARDS
            ELSE
              FLGS(K)=-1
              ERRNO=13
              CALL ERRPRT(ERRIO,ERRNO,IOVER,IDLST(K))
            ENDIF
          ELSE
            IREC=XSREC(K)
            IF((K+1).GT.NHDR)THEN
              EREC=NCARDS
            ELSE
              EREC=XSREC(K+1)-1
            ENDIF
          ENDIF
          IF(FLGS(K).EQ.-1) GO TO 25
          IF(BRNF.NE.FLGS(K))THEN
            MATCH=0
            IOLD=I
            NSTOP=NCOMP
 30         CONTINUE
              IF(I.LT.NSTOP)THEN
                I=I+1
              ENDIF
              IF(FLGS(K).EQ.COMP(6,I))THEN
                MATCH=1
                BRNF=INT(COMP(6,I))
              ENDIF
              IF(MATCH.NE.1.AND.I.GE.NCOMP.AND.NCOMP.NE.1)THEN
                NSTOP=IOLD
                I=0
              ENDIF
            IF(MATCH.NE.1.AND.I.LT.NSTOP) GO TO 30
C           compute area knots for branch
Cwrite            WRITE(*,*)'MATCH, BRANCH ',MATCH,BRANCH,I
            IF(I.EQ.0)THEN
              I=1
              NSTOP=NCOMP
              MATCH=1
            ENDIF
Cwrite            WRITE(*,*)'MATCH, BRANCH AFTER IF',MATCH,BRANCH,I
Cwrite            READ(*,*)IXXXK
            IF(COMP(1,I).LT.2.AND.MATCH.EQ.1) THEN
              NKTS=INT(COMP(4,I))
              IF(TBTYP(1).EQ.5) NKTS=25
              CALL KNOTS(NKTS,COMP(2,I),COMP(3,I),COMP(5,I),X,ERR)
            ELSE IF(MATCH.NE.1) THEN
              ERRNO=16
              CALL ERRPRT(ERRIO,ERRNO,FLGS(K),IDLST(K))
            ENDIF
          ENDIF
          IF(MATCH.EQ.0) GO TO 25
          CALL WSPCDS(FTEMP,IREC,EREC,S,G,N,NSA,XSA,NRUF,RUF,NSIN,
     #                SIN,SECID,SRD,SI,EDF)
          IF(EDF.GT.1) FLGS(K)=-1*EDF
          IF(EDF.GT.0) GO TO 25
          IF(COMP(1,I).GE.2)THEN
            J=IXMIN(G,N)
            MINDEP=G(J)+COMP(2,I)
            MAXDEP=G(J)+COMP(3,I)
            NKTS=INT(COMP(4,I))
            IF(TBTYP(1).EQ.5) NKTS=25
            CALL KNOTS(NKTS,MINDEP,MAXDEP,COMP(5,I),X,ERR)
            DO 20 II=1,NKTS
          CALL PROPER(S,G,N,NSA,XSA,X(II),NRUF,RUF,SIN,POUT,SUBPRP
     #     ,KXND)
        IF(TBTYP(1).EQ.2) THEN
          IF(II.EQ.1)THEN
            DATUM=POUT(13,1)-POUT(14,1)
            WRITE(TBUNT,210) SECID,SRD,DATUM
 210        FORMAT('HY',1X,A16,1X,F10.1,1X,F10.2)
          ENDIF
          CALL HYDOUT(TBUNT,KTS,POUT)
        ELSE IF(TBTYP(1).EQ.3) THEN
          CALL OUTTAB(TBUNT,KTS,NOUT,OPOUT,POUT)
        ELSE IF(TBTYP(1).EQ.1) THEN
          CALL BRNOUT(TBUNT,KTS,POUT)
        ELSE IF(TBTYP(1).EQ.4) THEN
          CALL SLPOUT(TBUNT,SRD,SECID,KTS,POUT,NSA,SUBPRP)
        ELSE IF(TBTYP(1).EQ.5) THEN
          CALL CULOUT(SRD,SECID,II,NKTS,POUT)
        ENDIF
 20         CONTINUE
          ELSE
            DO 15 II=1,NKTS
              CALL STAGE(S,G,N,NSA,XSA,X(II),NRUF,RUF,SIN,POUT
     #                   ,SUBPRP,KXND,ERFG)
              IF(ERFG.NE.0)THEN
                ERRNO=13+ERFG
                CALL ERRPRT(ERRIO,ERRNO,IOVER,SECID)
              ELSE
        IF(TBTYP(1).EQ.2) THEN
          IF(II.EQ.1)THEN
            DATUM=POUT(13,1)-POUT(14,1)
            WRITE(TBUNT,210) SECID,SRD,DATUM
          ENDIF
          CALL HYDOUT(TBUNT,KTS,POUT)
        ELSE IF(TBTYP(1).EQ.3) THEN
          CALL OUTTAB(TBUNT,KTS,NOUT,OPOUT,POUT)
        ELSE IF(TBTYP(1).EQ.1) THEN
          CALL BRNOUT(TBUNT,KTS,POUT)
        ELSE IF(TBTYP(1).EQ.4) THEN
          CALL SLPOUT(TBUNT,SRD,SECID,KTS,POUT,NSA,SUBPRP)
        ELSE IF(TBTYP(1).EQ.5) THEN
          CALL CULOUT(SRD,SECID,II,NKTS,POUT)
        ENDIF
              ENDIF
 15         CONTINUE
          ENDIF
 25   CONTINUE
      ENDIF
C
      IF(ISEC.GT.0.AND.NCOMP.EQ.0) THEN
      DO 35 K=1,ISEC
        EDF=0
        IF(NOHP.EQ.-1)THEN
C
        IF(SECPRP(2,K).NE.0)THEN
          RATIO=1.0
          NKTS=INT( (SECPRP(3,K)-SECPRP(1,K))/SECPRP(2,K) )+1
        ELSE
          NKTS=1
        ENDIF
        CALL KNOTS(NKTS,SECPRP(1,K),SECPRP(3,K),RATIO,X,ERR)
        IREC=FNDXSC(ELID(K),FTEMP(1),XSREC,NHDR)
        IF(IREC.GT.0) THEN
          IF((IREC+1).GT.NHDR) THEN
            EREC=NCARDS
          ELSE
            EREC=XSREC(IREC+1)-1
          ENDIF
          IREC=XSREC(IREC)
          EDF=0
          CALL WSPCDS(FTEMP,IREC,EREC,S,G,N,NSA,XSA,NRUF,RUF,NSIN
     #                ,SIN,SECID,SRD,SI,EDF)
        ENDIF
C
        ELSE
          KK=0
          SL1=16
          IREC=XSREC(K)
          IF((K+1).GT.NHDR) THEN
            EREC=NCARDS
          ELSE
            EREC=XSREC(K+1)-1
          ENDIF
          IREC=XSREC(K)
          EDF=0
          CALL WSPCDS(FTEMP,IREC,EREC,S,G,N,NSA,XSA,NRUF,RUF,NSIN
     #                ,SIN,SECID,SRD,SI,EDF)
          IF(EDF.GT.0) GO TO 35
 50       CONTINUE
          KK=KK+1
          NOMAT=MATSTR(SECID,ELID(KK),SL1,SL1)
          IF(NOMAT.NE.1.AND.KK.LT.NOHP) GO TO 50
          IF(NOMAT.EQ.1) THEN
C           HP record exists for that secid, use it for w.s. el
            IF(SECPRP(2,K).NE.0)THEN
              RATIO=1.0
              NKTS=INT((SECPRP(3,K)-SECPRP(1,K))/SECPRP(2,K) )+1
            ELSE
              NKTS=1
            ENDIF
            CALL KNOTS(NKTS,SECPRP(1,K),SECPRP(3,K),RATIO,X,ERR)
          ELSE IF(S(1).EQ.S(N)) THEN
            NKTS=IXMAX(G,N)
            X(1)=G(NKTS)+0.5
            NKTS=1
          ELSE
C           error, no HP record for nonenclosed section -SAC error
            ERRNO=19
            CALL ERRPRT(ERRIO,ERRNO,IOVER,SECID)
          ENDIF
        ENDIF
C
          IF(EDF.GT.1) FLGS(K)=-1*EDF
          IF(EDF.GT.0) GO TO 35
            DO 45 II=1,NKTS
           CALL PROPER(S,G,N,NSA,XSA,X(II),NRUF,RUF,SIN,POUT,SUBPRP,
     #                 KXND)
C           * write property tables *
          IF(TBTYP(1).EQ.2) THEN
            IF(II.EQ.1)THEN
              DATUM=POUT(13,1)-POUT(14,1)
              WRITE(TBUNT,210) SECID,SRD,DATUM
            ENDIF
            CALL HYDOUT(TBUNT,KTS,POUT)
          ELSE IF(TBTYP(1).EQ.3) THEN
            CALL OUTTAB(TBUNT,KTS,NOUT,OPOUT,POUT)
          ELSE IF(TBTYP(1).EQ.1) THEN
            CALL BRNOUT(TBUNT,KTS,POUT)
          ELSE IF(TBTYP(1).EQ.4) THEN
            CALL SLPOUT(TBUNT,SRD,SECID,KTS,POUT,NSA,SUBPRP)
          ELSE IF(TBTYP(1).EQ.5) THEN
            CALL CULOUT(SRD,SECID,II,NKTS,POUT)
          ENDIF
 45         CONTINUE
c        ELSE
c          ERRNO=13
c          CALL ERRPRT(ERRIO,ERRNO,IOVER,ELID(K))
c        ENDIF
35    CONTINUE
      ENDIF
      ENDIF
999   CONTINUE
       FILST=3
C
      RETURN
      END
C
C
      SUBROUTINE WSPDA
     I                (FCARD,FTEMP,
     O                 NCOMP,COMP,ISEC,SECPRP,ELID,NCARDS,NHDR,TBTYP,
     O                 XSREC,NOUT,OPOUT,TITLES,SI,ERFG)
C
C     + + + PURPOSE + + +
C     Reads WSPRO file into a direct access temporary file and initializes
C     the computational arrays.
C
C     + + + PARAMETERS + + +
      INTEGER NXSEC
      PARAMETER (NXSEC=200)
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER FCARD,FTEMP(2),NCOMP,ISEC,NCARDS,NHDR,TBTYP(3),NOUT,ERFG,
     #        XSREC(NXSEC),OPOUT(17),SI
      REAL COMP(6,5),SECPRP(3,NXSEC)
      CHARACTER*75 TITLES(3)
      CHARACTER*80 CDUM
      CHARACTER*16 ELID(NXSEC)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER NCNT,NCHAR,MAX,ERRIO,ERRNO,NTIT
      REAL VAR(5)
      CHARACTER*75 CBUF
      CHARACTER*3 CODE
      CHARACTER*1 OPT
      CHARACTER*16 BLKOUT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     SI      - unit flag input/output 0-fts/fts 1-ms/ms 2-fts/ms 3-ms/fts
C     FCARD   - unit on which WSPRO file is openned
C     FTEMP(1)   - unit on which temp DA is openned
C     NCOMP   - no. of general compation cards read
C     COMP    - arra y of min, max, geometric ratio and increment sizes
C               for determing values at which hydraulic properties will
C               be computed
C     ISEC    - no. of cards for determining stages at which hydraulic
C               properties will be computed.
C     ELID    - section I.D.
C     NCARDS  - no. of cards read from WSPRO file
C     NHDR    - no. of cross sections read
C     TITLES  - wspro title records, t1, t2, & t3
C
C     + + + LOCAL VARIABLES + + +
C      
C     + + + EXTERNALS + + +
      CHARACTER*80 BLOT
      EXTERNAL SETOUT,PCHRR,BLOT
C
C     + + + INITIALIZATIONS + + +
      DATA ERRNO / 12 /
C
C     + + + FORMATS + + +
 100  FORMAT(A3,1X,A1,A75)
C
C
      ERRIO=FTEMP(2)
      NTIT=0
      DO 2 I=1,3
        CDUM=BLOT(TITLES(I))
        TITLES(I)=CDUM(1:75)
 2    CONTINUE
      MAX=5
      BLKOUT='                '
 5    CONTINUE
      READ(FCARD,100,END=998) CODE,OPT,CBUF
      NCARDS=NCARDS+1
      NCHAR=75
      IF(CODE(1:1).EQ.'*')THEN
        CALL SETOUT(CODE,OPT,CBUF,NCHAR,ERRIO,TBTYP,NCOMP,
     #              COMP,NOUT,OPOUT) 
      ELSE IF(CODE.EQ.'HP ')THEN
C     * found WSPRO hydraulic prop card *
        ISEC=ISEC+1
        ELID(ISEC)=CBUF(1:5)//BLKOUT(1:11)
        CALL PCHRR(CBUF(6:75),NCHAR,MAX,NCNT,VAR)
        IF(NCNT.GE.3)THEN
          SECPRP(1,ISEC)=VAR(1)
          SECPRP(2,ISEC)=VAR(2)
          SECPRP(3,ISEC)=VAR(3)
        ELSE IF(NCNT.LT.3) THEN
          SECPRP(1,ISEC)=VAR(1)
          SECPRP(2,ISEC)=0
          SECPRP(3,ISEC)=0
        ENDIF
      ELSE IF(CODE.EQ.'T1 '.OR.CODE.EQ.'T2 '.OR.CODE.EQ.'T3 '
     #    .AND.NTIT.LT.3) THEN
        NTIT=NTIT+1
        TITLES(NTIT)=CBUF(1:75)
      ELSE IF(CODE.EQ.'SI')THEN
        CALL PCHRR(CBUF(1:75),NCHAR,MAX,NCNT,VAR)
        SI=INT(VAR(1))
      ELSE
        IF(CODE.EQ.'XS '.OR.CODE.EQ.'XT '.OR.CODE.EQ.'AS '.OR.CODE.EQ.
     #   'BR '.OR.CODE.EQ.'CV '.OR.CODE.EQ.'SD '.OR.CODE.EQ.'XR ')
     #   THEN
          NHDR=NHDR+1
          IF(NHDR.GT.NXSEC)THEN
            ERFG=1
            CALL ERRPRT(ERRIO,ERRNO,NHDR,BLKOUT)
            GO TO 998
          ENDIF
          XSREC(NHDR)=NCARDS
        ENDIF
      ENDIF
        WRITE(FTEMP(1),100,REC=NCARDS)CODE,OPT,CBUF
      GO TO 5
 998  CONTINUE
      WRITE(FTEMP(1),'(I4)',REC=1)NCARDS
C
      IF(SI.EQ.1.OR.SI.EQ.3)THEN
C       convert from meter-seconds to feet-seconds
        IF(ISEC.GT.0)THEN
          DO 11 I=1,ISEC
            SECPRP(1,I)=SECPRP(1,I)*(100./30.48)
            SECPRP(2,I)=SECPRP(2,I)*(100./30.48)
            SECPRP(3,I)=SECPRP(3,I)*(100./30.48)
 11       CONTINUE
        ENDIF
        IF(NCOMP.GT.0)THEN
          DO 10 I=1,NCOMP
            IF(COMP(1,I).EQ.1)THEN
C             appears to convert square meters to square ft
              COMP(2,I)=COMP(2,I)*(100./30.48)*(100./30.48)
              COMP(3,I)=COMP(3,I)*(100./30.48)*(100./30.48)
            ELSE IF(COMP(1,I).EQ.2)THEN
C             appears to convert meters to feet
              COMP(2,I)=COMP(2,I)*(100./30.48)
              COMP(3,I)=COMP(3,I)*(100./30.48)
            ENDIF
 10       CONTINUE
        ENDIF
      ENDIF
      RETURN
      END
C
C
      SUBROUTINE KNOTS
     I                (KNTS,AMIN,AMAX,RATIO,
     O                 KNATS,ERR)
C
C     + + + PURPOSE + + +
C     LLDelongs routine for using a geometric selection of computation
C     points
C
C     + + + ARGUMENT DEFINITIONS + + +
C     KNTS   - no. of values to compute
C     AMIN   - minimun value
C     AMAX   - maximum value
C     RATIO  - geometric ratio
C     KNATS  - array of values
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER KNTS
      REAL AMIN,AMAX,RATIO,KNATS(KNTS)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER N,ERR,KNOTSM
      REAL XDIF,DX,ADX
C
C     + + + INTRINSICS + + +
      INTRINSIC ABS
C
C
      ERR=0
      KNOTSM=KNTS-1
      XDIF=AMAX-AMIN
      KNATS(KNTS)=AMAX
      KNATS(1)=AMIN
      IF(KNTS.GT.1) THEN
      IF(RATIO.GT.0.0) THEN
        IF(ABS(RATIO-1.0).GT.1.0E-06) THEN
          ADX=XDIF*(1.0-RATIO)/(1.0-RATIO**KNOTSM)
        ELSE
          ADX=XDIF/KNOTSM
        ENDIF
        DO 10 N=2,KNOTSM
          DX=ADX*RATIO**(N-2)
          KNATS(N)=KNATS(N-1)+DX
 10     CONTINUE
      ELSE
        ERR=1
      ENDIF
      ENDIF
C
      RETURN
      END
C
C
      SUBROUTINE HYDOUT
     I                 (TBUNT,NOUT,POUT)
C
C     + + + PURPOSE + + +
C     Writes sequential file of the hydraulic properties needed for Hydraux.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER NOUT,TBUNT
      REAL POUT(17,NOUT)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     TBUNT -  file unit on which table is written
C     DATUM - cross section datum
C     NOUT  - no. of poihnts at which hydraulic properties are computed
C     POUT  - array of hydraulic properties
C
C     + + + LOCAL VARIABLES + + +
      INTEGER I
C
C     + + + OUTPUT FORMATS + + +
C 210  FORMAT('HY',1X,A16,1X,F10.1,1X,F10.2)
 200  FORMAT('DP',1X,F10.2,2(1X,E13.6),3(1X,F5.2),2(1X,F7.1))
C
C
      DO 10 I=1,NOUT
        WRITE(TBUNT,200) POUT(14,I),POUT(3,I),POUT(15,I),POUT(10,I),
     #                   POUT(7,I),POUT(8,I),POUT(2,I),POUT(6,I)
 10   CONTINUE
C
      RETURN
      END
C
      SUBROUTINE SLPOUT
     I                 (TBUNT,RD,XSNAME,NKTS,POUT,NSA,SUBPRP)
C
C     + + + PURPOSE + + +
C     Writes sequential file of the hydraulic properties needed for slope
C     area program.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER NKTS,TBUNT,NSA
      REAL RD,POUT(17,NKTS),SUBPRP(7,NSA)
      CHARACTER  XSNAME*16
C
C     + + + ARGUMENT DEFINITIONS + + +
C     TBUNT -  file unit on which table is written
C     RD   - reference distance in river feet.
C     NKTS  - no. of points at which hydraulic properties are computed
C     POUT  - array of hydraulic properties
C     XSNAME - cross section name
C     SUBPRP - subarea properties, first index denote property type, second
C              index the subarea
C
C     + + + LOCAL VARIABLES + + +
      INTEGER I
C
C     + + + FORMATS + + +
200   FORMAT (A16,1X,F10.1,1X,F10.2,3(1X,E13.6)
     #        ,2(1X,F7.2))
210   FORMAT (2(1X,E13.6),1X,F5.3)
220   FORMAT (I5)
230   FORMAT (6(E13.6,1X),F6.4)
C
      DO 10 I=1,NKTS
      WRITE(TBUNT,200) XSNAME,RD,POUT(13,I),POUT(1,I),POUT(3,I),
     #                 POUT(9,I),POUT(16,I),POUT(17,I)
      WRITE(TBUNT,210)POUT(2,I),POUT(6,I),POUT(11,I)
      WRITE(TBUNT,220)NSA
      WRITE(TBUNT,230)((SUBPRP(K,INSA),K=1,7),INSA=1,NSA)
10    CONTINUE
C
      RETURN
      END
C
C
      SUBROUTINE BRNOUT
     I                 (TBUNT,NOUT,POUT)
C
C     + + + PURPOSE + + +
C     Writes sequential file of the hydraulic properties needed for Branch.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER NOUT,TBUNT
      REAL POUT(17,NOUT)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     TBUNT -  file unit on which table is written
C     NOUT  - no. of poihnts at which hydraulic properties are computed
C     POUT  - array of hydraulic properties
C
C     + + + LOCAL VARIABLES + + +
      INTEGER I
C
C     + + + OUTPUT FORMATS + + +
 210  FORMAT(1F10.3,2F10.1)
C
C
      DO 10 I=1,NOUT
        WRITE(TBUNT,210) POUT(13,I),POUT(1,I),POUT(2,I)
 10   CONTINUE
C
      RETURN
      END

      SUBROUTINE SETOUT
     I                 (CODE,COPT,CBUF,NCHAR,ERRIO,
     O                  TBTYP,NCOMP,COMP,NOUT,OPOUT)
C
C     + + + PURPOSE + + +
C     Sets the arrays determining output tables and entire data set
C     computations.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER TBTYP(3),NCOMP,NOUT,OPOUT(17),NCHAR,ERRIO
      REAL COMP(6,5)
      CHARACTER*(*) CBUF
      CHARACTER*3 CODE
      CHARACTER*1 COPT
      CHARACTER*16 BLKOUT
C
C     + + + LOCAL VARIABLES + + +
      INTEGER OPT,MAX,NCNT,I,ERRNO
      REAL VAR(15),DVAR
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CODE  - card type identifier
C     COPT   - option number
C     CBUF  - character array
C     NCHAR - no. of characters allowable in string
C     TBTYP - table type to be outputted, 1-Branch 2-Hydraux 3-custom
C     NCOMP - no. of general computation cards
C     COMP  - array of min, max, geometeric ratio and increments that are
C             used to determine the values at which the hydraulice properties
C             will be computed.
C     NOUT  - no. of variables out for custom table
C     OPOUT - name flags of variables out for custom tables
C
C     + + + EXTERNALS + + +
      EXTERNAL PCHRR
C
      INTRINSIC INT
C
C     + + + INITIALIZATIONS + + +
      DATA BLKOUT / '                ' /
C
      IF(CODE.EQ.'*TT')THEN
C     * found table type card *
         I=1
        READ(COPT,'(I1)') OPT
        IF(OPT.GE.0.AND.OPT.LE.5) THEN
          TBTYP(I)=OPT
        ELSE
          TBTYP(I)=0
        ENDIF
      ELSE IF(CODE.EQ.'*OT')THEN
C     * found custom table card *
        MAX=15
       CALL PCHRR(CBUF,NCHAR,MAX,NOUT,VAR)
       DO 15 I=1,NOUT
         OPOUT(I)=INT(VAR(I))
 15    CONTINUE
      ELSE IF(CODE.EQ.'*PA'.OR.CODE.EQ.'*PD')THEN
C     * found computation range card  field 1, min; 2, max, 3, ratio,
C       4, no. of points;, 5, branch no. *
       MAX=5
C      set default branch flag and default ratio of 1
       VAR(3)=1.0
       VAR(4)=2.0
       VAR(5)=1.0
       CALL PCHRR(CBUF,NCHAR,MAX,NCNT,VAR)
       NCOMP=NCOMP+1
       IF(NCOMP.GT.5) THEN
C        exceeds no. of allowable computation records
         ERRNO=1
         CALL ERRPRT(ERRIO,ERRNO,NCOMP,BLKOUT)
         GO TO 998
       ENDIF
       IF (VAR(2).LE.VAR(1)) THEN
C        range is entered in descending order, not allowed
C        swap the range into ascending order
         ERRNO=20
         CALL ERRPRT(ERRIO,ERRNO,NCOMP,BLKOUT)
         DVAR=VAR(2)
         VAR(2)=VAR(1)
         VAR(1)=DVAR
       ENDIF
       IF(VAR(3).LE.0) THEN
C        ratio value
         ERRNO=21
         CALL ERRPRT(ERRIO,ERRNO,NCOMP,BLKOUT)
         VAR(3)=1.0
       ENDIF
C      swap the ratio and no. of points variables to maintain
C      the way the old array (prior to 10-94) was setup to
C      minimize code changes.
       DVAR=VAR(3)
       VAR(3)=VAR(4)
       VAR(4)=DVAR
       DO 10 I=2,6
         COMP(I,NCOMP)=VAR(I-1)
 10    CONTINUE
       IF(CODE.EQ.'*PA') COMP(1,NCOMP)=1
       IF(CODE.EQ.'*PD') COMP(1,NCOMP)=2
      ENDIF
 998  CONTINUE
C
      RETURN
      END
C
C
      SUBROUTINE WSPSEQ
     I                 (FTEMP,FCARD,NCARDS)
C
C     + + + PURPOSE + + +
C     Writes temporary DA file to a sequential file that has
C     basic WSPRO format.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER FTEMP,FCARD,NCARDS
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FTEMP  - file no. of temporary DA file
C     FCARD  - file no. of sequential output file for data
C     NCARDS - no. of records in DA file
C
C     + + + LOCAL VARIABLES + + +
      INTEGER IREC
      CHARACTER*80 CBUF
C
100   FORMAT(A80)
      IREC=1
10    CONTINUE
        READ(FTEMP,100,REC=IREC) CBUF
        WRITE(FCARD,100) CBUF
        IREC=IREC+1
      IF(IREC.LE.NCARDS) GO TO 10
C
      RETURN
      END
C
C
      SUBROUTINE OUTTAB
     I                 (FTAB,NKTS,NOUT,OPOUT,POUT)
C
C     + + + PURPOSE + + +
C     Writes table of hydraulic properties to unit FTAB
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER FTAB,NKTS,NOUT,OPOUT(17)
      REAL POUT(17,NOUT)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FTAB   - file no. for output table of hydraulic properties
C     NKTS   - no. of points at which properties are computed
C     NOUT   - no. of hydraulic properties to output
C     OPOUT  - array containing intger flag list of variable names
C              outputtted to custom table.
C     POUT   - (i,k) array of computed hydraulic properties where i
C              is the index of the property and k is the index of
C              either stage or area. max k = nkts, max i = 15
C
C     + + + LOCAL VARIABLES + + +
      INTEGER ECOL,IREM,K,I
      CHARACTER*14 PRNAME(15)
C
C     + + + INITIALIZATIONS + + +
      DATA PRNAME / 'CONVEYANCE    ' , 'WIDTH         ' ,
     #              'AREA          ' , 'PHIM          ' ,
     #              'PHIV          ' , 'WETTED PERIM  ' ,
     #              'AV. SINUOSITY ' , 'WT. SINUOSITY ' ,
     #              'ALPHA         ' , 'BETA          ' ,
     #              'COMP. ROUGH   ' , 'Y CENTROID    ' ,
     #              'STAGE         ' , 'DEPTH         ',
     #              'WT. CONVEYANCE' /
C
C     + + + OUTPUT FORMATS + + +
 200  FORMAT(5(2X,A14))
 210  FORMAT(5(2X,F14.3))
C
C
      IREM=NOUT
 15   CONTINUE
      IF(NOUT.GT.5)THEN
        ECOL=5
        IREM=IREM-5
      ELSE
        ECOL=IREM
        IREM=0
      ENDIF
      DO 10 K=1,NKTS
        IF(K.LT.2)WRITE(FTAB,200)(PRNAME(OPOUT(I)),I=1,ECOL)
        WRITE(FTAB,210)(POUT(OPOUT(I),K),I=1,ECOL)
  10  CONTINUE
      IF(IREM.GT.0) GO TO 15
C
      RETURN
      END
C
C
      CHARACTER*(80) FUNCTION BLOT(STRING)
C
C     + + + PURPOSE + + +
C     Blanks out a character string less than 80 characters
C
C     + + + DUMMY ARGUMENTS + + +
      CHARACTER STRING*(*)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     STRING - string to blank out
C
C     + + + LOCAL VARIABLES + + +
      INTEGER K,I
      CHARACTER*1 BLK
C
C     + + + INTRINSIC + + +
      INTRINSIC LEN
C
C
      BLK=' '
      I=LEN(STRING)
      IF(I.GT.80) I=80
      DO 10 K=1,I
        STRING(K:K)=BLK
 10   CONTINUE
      BLOT(1:I)=STRING(1:I)
C
      RETURN
      END
C
C
      SUBROUTINE SRPBLK(STRING,SLEN)
C
C     + + + PURPOSE + + +
C     Removes leading blanks from character strings
C
C     + + + DUMMY ARGUMENTS + + +
C     CHARACTER STRING*(*)
      INTEGER SLEN
      CHARACTER*(16) STRING
C
C     + + + LOCAL VARIABLES + + +
      INTEGER I,FLG
C
C     + + + INTRINSIC + + +
      INTRINSIC LEN
C
C
      I=0
      FLG=0
 10   CONTINUE
        I=I+1
        IF(STRING(I:I).EQ.' ') THEN
          STRING=STRING(2:SLEN)//' '
        ELSE
          FLG=1
        ENDIF
      IF(FLG.EQ.0.AND.I.LT.SLEN) GO TO 10
C
      RETURN
      END
C
C
      INTEGER FUNCTION MATSTR(STRNG0,STRNG1,SLN0,SLN1)
C
C     + + + PURPOSE + + +
C     Checks for string matching. Returns a 1 if string
C     matches.  Strips leading blanks from strings before
C     comparing.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER SLN0,SLN1
      CHARACTER*(16) STRNG0
      CHARACTER*(16) STRNG1
C
C     + + + ARGUMENT DEFINITONS + + +
C     STRNG0 - string to compare
C     STRNG1 - string to compare
C
C
C     + + + LOCAL VARIABLES + + +
      INTEGER MXLEN,I
C
C     + + + EXTERNAL + + +
      EXTERNAL SRPBLK
C
C     + + + INTRINSIC + + +
      INTRINSIC LEN
C
C
      MATSTR=1
      CALL SRPBLK(STRNG0,SLN0)
      CALL SRPBLK(STRNG1,SLN1)
      I=0
      MXLEN=SLN0
 10   CONTINUE
        I=I+1
        IF(STRNG0(I:I).NE.STRNG1(I:I)) MATSTR=0
      IF(MATSTR.EQ.1.AND.I.LT.MXLEN) GO TO 10
C
      RETURN
      END
C
C
      INTEGER FUNCTION FNDXSC(SECID,FTEMP,XSREC,NHDR)
C
C     + + + PURPOSE + + +
C     Locates cross section data given the secid.  Returns the DA pointer.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER FTEMP,NHDR,XSREC(NHDR)
      CHARACTER*16 SECID
C
      CHARACTER*16 IDLST,ELID
C    + + + ARGUMENT DEFINITONS + + +
C     FTEMP  - DA file no. containing cross section data
C     NHDR   - no. of cross sections in DA file
C     XSREC  - (i), integer array of DA points to cross section
C              data. max i = NHDR.
C     SECID  - section i.d. to match against or locate in DA file
C
C     + + + LOCAL VARIABLES + + +
      INTEGER I,MATSTR,SLN0,SLN1,NREC
      LOGICAL NOMAT
      CHARACTER*3 CODE
C
C     + + + EXTERNAL + + +
      EXTERNAL MATSTR
C
C     + + + INPUT FORMAT + + +
100   FORMAT(5X,A5)
110   FORMAT(A3,1X,A16)
C
      I=0
      IDLST=SECID
      SLN0=LEN(IDLST)
 10   CONTINUE
        I=I+1
        READ(FTEMP,100,REC=XSREC(I)) ELID
        SLN1=LEN(ELID)
        NOMAT=MATSTR(IDLST,ELID,SLN0,SLN1).NE.1
        IF(NOMAT)THEN
          NREC=XSREC(I)+1
           READ(FTEMP,110,REC=NREC)CODE,ELID
           SLN1=LEN(ELID)
          IF (CODE.EQ.'*ID')THEN
            NOMAT=MATSTR(IDLST,ELID,SLN0,SLN1).NE.1
          ENDIF
        ENDIF
      IF(NOMAT.AND.I.LT.NHDR) GO TO 10
      IF(NOMAT) THEN
        FNDXSC=-1
      ELSE
        FNDXSC=I
      ENDIF
C
      RETURN
      END
C
      SUBROUTINE HYDDA
     I                 (FTUNIT,FTEMP,IREC,
     O                  NCOMP,COMP,NHDR,SECPRP,SECID,TBTYP,XSREC
     O                  ,NOUT,OPOUT,ERFG)
C
C     + + + PURPOSE + + +
C     Writes hydraux data deck into a temporary DA file using Wspro
C     data format.
C
C     * * * PARAMETERS * * *
      INTEGER NXSEC
      PARAMETER (NXSEC=200)
C
C     * * * DUMMY ARGUMENTS * * *
      INTEGER FTUNIT,IREC,FTEMP(2),NCOMP,NHDR,ERFG,TBTYP(3),NOUT
     #       ,OPOUT(17),XSREC(NXSEC)
      REAL COMP(6,5),SECPRP(3,NXSEC)
      CHARACTER*16 SECID(NXSEC)
C
C     * * * ARGUMENT DEFINITIONS * * *
C     FTUNIT - writes hydraux data into a temp DA file using WSPRO image
C     FTUNIT - unit on which hydraux file is openned
C     FTEMP(1)  - unit on which temp DA file is openned
C     FTEMP(2)  - unit on which error messages are printed
C     IREC   - starting record on DA where hydraux data is to be placed
C     NCOMP  - no. of general computation cards
C     COMP   - array of min, max, geometric ratio and increments that
C              are used to determine the values at which the hydraulic
C              properties will be computed.
C     NHDR   - no. of cross sections in temp file
C     SECPRP - array of min, max and increment size used to determine the
C              stages at which hydraulic properties will be computed.
C     SECID  - section I.D.,
C     TBTYP  - Table type required for output, 1-Branch 2-Hydraux 3- custom
C     XSREC  - DA record pointers to start of each cross section
C     NOUT   - no. of variables out for custom table
C     OPOUT  - name of varibles out for custom tables
C     ERFG   - error flag, not in use.
C
C     * * * LOCAL VARIABLES * * *
      INTEGER FLUSH,ERRFLG,NIN,I,ERRIO,ERRNO
     #        ,NSA,SLEN,NCHAR
      REAL WORK(200),RMILE,SRD
      CHARACTER*3 CODE,GRCD,SACD,NCD,NDPCD,SINCD,SNDPCD
      CHARACTER*5 OPCAR,OPT
      CHARACTER*16 DUM,BLKOUT
      CHARACTER*64 CBUF
      CHARACTER*1 OPTWSP
C
C     + + + EXTERNALS + + +
      EXTERNAL SETOUT,VREADV,CDOUT
C
C     * * * FORMATS * * *
100   FORMAT(A5,1X,A3,1X,A1,5X,A64)
101   FORMAT(A16,F6.2,A56)
200   FORMAT('XS',3X,A5,F10.1)
201   FORMAT('*ID ',A16)
210   FORMAT('HP',' 1 ',A5,3F10.4)
220   FORMAT(A3,1X,A1,5X,A64)
C     * * * DATA INITIALIZATIONS * * *
      DATA OPT /'$OPTS'/
C
      DATA GRCD,SACD,NCD,NDPCD,SINCD,SNDPCD/ 'GR ','SA ','N  ','ND ',
     #        '*SN','*SD' /
C      DATA HPCD / 'HP ' /
      DATA BLKOUT / '                ' /
      DATA ERRNO / 12 /
      ERRIO=FTEMP(2)
      NCHAR=64
      ERRFLG=0
40    CONTINUE
      IREC=IREC+1
 15   CONTINUE
      FLUSH=0
      READ(FTUNIT,100,END=999) OPCAR,CODE,OPTWSP,CBUF
      IF(OPCAR.NE.OPT) FLUSH=1
      IF(CODE(1:1).EQ.'*') THEN
        WRITE(FTEMP(1),220,REC=IREC) CODE,OPTWSP,CBUF
        IREC=IREC+1
        CALL SETOUT(CODE,OPTWSP,CBUF,NCHAR,ERRIO,TBTYP,NCOMP,COMP,
     #              NOUT,OPOUT)
      ENDIF
      IF(FLUSH.EQ.1) GO TO 15
C
      NHDR=NHDR+1
      IF(NHDR.GT.NXSEC) THEN
        ERFG=1
        CALL ERRPRT(ERRIO,ERRNO,NHDR,BLKOUT)
        GO TO 999
      ENDIF
      SECID(NHDR)=BLKOUT
      DUM='                '
      READ(FTUNIT,101) DUM(1:16),RMILE
      SECID(NHDR)=DUM
C
      SRD=RMILE*5280.
      WRITE(FTEMP(1),200,REC=IREC) SECID(NHDR),SRD
      XSREC(NHDR)=IREC
      IREC=IREC+1
      WRITE(FTEMP(1),201,REC=IREC) SECID(NHDR)
      SLEN=200
      CALL VREADV(FTUNIT,WORK,SLEN,NIN)
C
      CALL CDOUT(FTEMP(1),IREC,NIN,WORK,GRCD)
C
      SLEN=19
      CALL VREADV(FTUNIT,WORK,SLEN,NSA)
      CALL CDOUT(FTEMP(1),IREC,NSA,WORK,SACD)
C
C     * read roughness values *
      SLEN=40
      CALL VREADV(FTUNIT,WORK,SLEN,NOUT)
      CALL CDOUT(FTEMP(1),IREC,NOUT,WORK,NCD)
C
C     * read roughness depths *
      IF(NOUT.GT.NSA+1)THEN
        CALL VREADV(FTUNIT,WORK,SLEN,NOUT)
        CALL CDOUT(FTEMP(1),IREC,NOUT,WORK,NDPCD)
      ENDIF
C
C     * sinuosity values *&
      CALL VREADV(FTUNIT,WORK,SLEN,NOUT)
      CALL CDOUT(FTEMP(1),IREC,NOUT,WORK,SINCD)
C     * read sinousity depths *
      IF(NOUT.GT.NSA+1) THEN
        CALL VREADV(FTUNIT,WORK,SLEN,NOUT)
        CALL CDOUT(FTEMP(1),IREC,NOUT,WORK,SNDPCD)
      ENDIF
C     * read computation points for section *
      SLEN=4
      NOUT=0
      CALL VREADV(FTUNIT,WORK,SLEN,NOUT)
C     CALL CDOUT(FTEMP(1),IREC,NOUT,WORK,HPCD)
      IREC=IREC+1
      WRITE(FTEMP(1),210,REC=IREC) SECID(NHDR),(WORK(I),I=1,3)
      DO 20 I=1,3
        SECPRP(I,NHDR)=WORK(I)
 20   CONTINUE
C
      IF(ERRFLG.NE.0) THEN
        PRINT*,'Reading error during HYDRAUX file access'
      ENDIF
      GO TO 40
999   CONTINUE
        IREC=IREC-1
        WRITE(FTEMP(1),'(I4)',REC=1) IREC
C
      RETURN
      END
C
      SUBROUTINE WSPCDS
     I                 (FTUNIT,IREC,EREC,S,G,N,NSA,XSA,NRUF,
     O                  RUF,NSIN,SIN,SECID,SRD,SI,EDFG)
C
C     + + +  PURPOSE + + +
C     Reads a single x-section from a WSPRO DA file into the arrays
C     several WSPRO card types are ignored, including
C     bridge, spur dike, road section and culvert header cards and
C     data.  Skew angle correction fixed on 1.17.97
C
C     + + + PARAMETERS + + +
      INTEGER NCORD,NSUBS,NSBS
      PARAMETER (NCORD=150, NSUBS=19, NSBS=40)
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER FTUNIT(2),IREC,EREC,NSIN,NRUF,N,NSA,EDFG,SI
      REAL S(NCORD),G(NCORD),XSA(NSUBS),RUF(3,NSBS),
     #     SIN(3,NSBS),SRD
      CHARACTER*16 SECID
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FTUNIT(1)  - unit on which temp DA file is openned
C     FTUNIT(2)  - unit on which error messages are printed
C     IREC    - starting record of cross section in DA file
C     EREC    - ending record of cross section in DA file
C     S       - station point
C     G       - ground elevation
C     N       - no. of station points and groung elevation pairs
C     NSA     - no. of subsection areas
C     XSA     - array of subsection righthand breakpoints.
C     NRUF    - no. of roughness values
C     RUF     - array of roughness data
C     NSIN    - no. of sinuosity values
C     SIN     - array of sinuosity data
C     SECID   - section I.D.
C     SRD     - cross section reference distance in feet
C     EDFG    - error flag
C
C     + + + LOCAL VARIABLES + + +
      INTEGER FLG,I,J,NCHAR,MAX,NCNT,TMPFLG,GRFG,IO,MATSTR,IXMIN,
     #        NFG,SAFG,ONRUF,NDEP,IJ,DFG,ONSA,ERRIO,ERRNO
     #        ,RREC,SINFG,DSFG,NSDEP,ODFG,IOVER,OTNIT
      REAL  VSLOPE,SKEW,VAR(40),YMIN,CORDAT
      REAL m_to_ft
      CHARACTER  CBUF*77,CHRDUM*77
      CHARACTER*3 CODE,COM,SINJNK
      CHARACTER*16 BLKOUT
C
C     + + + EXTERNALS + + +
      EXTERNAL PCHRR,MATSTR,IXMIN,FXARY
C
C     + + + INTRINSIC + + +
      INTRINSIC COS,FLOAT
C
C     + + + DATA INITIALIZATIONS + + +
      DATA COM / '   ' /
      DATA BLKOUT / '                ' /
      DATA IOVER / 1000 /
C
C     + + + INPUT FORMAT + + +
 100  FORMAT(A3,A77)
C
C
      ERRIO=FTUNIT(2)
      OTNIT=ABS(FTUNIT(2))
      NCHAR=70
      SKEW=1.0
      VSLOPE=0.0
      DSFG=0
      SINJNK='*SN'
      SINFG=0
      SAFG=0
      NDEP=0
      GRFG=0
      DFG=0
      ONRUF=0
      NFG=0
      ONSA=0
      NSDEP=0
      TMPFLG=0
      YMIN=0.0
      RREC=IREC
        DO 2 I=1,40
          SIN(1,I)=0.0
          SIN(2,I)=1.0
          SIN(3,I)=0.0
 2      CONTINUE
C
 15   CONTINUE
        FLG=0
        READ(FTUNIT(1),100,REC=RREC) CODE,CBUF
        WRITE(OTNIT,100) CODE,CBUF
        RREC=RREC+1
C       check for header card
        IF(CODE.EQ.'XS  '.OR.CODE.EQ.'XT '.OR.CODE.EQ.'AS '.OR.CODE.EQ.
     #     'BR '.OR.CODE.EQ.'CV '.OR.CODE.EQ.'SD '.OR.CODE.EQ.'XR ')
     #     THEN
          IF(N.GT.0)THEN
            IF(GRFG.EQ.0) THEN
C            adjust old x,y pairs
              DO 75 I=1,N
                S(I)=S(I)
                G(I)=G(I)+SRD*VSLOPE
 75           CONTINUE
            ENDIF
            IF(ONSA.EQ.NSA.AND.ONRUF.EQ.NRUF.OR.ONSA.EQ.0) THEN
              IF(SAFG.EQ.0) THEN
                DO 80 I=1,NSA
                  XSA(I)=XSA(I)
 80             CONTINUE
              ENDIF
              IF(DFG.EQ.0) THEN
                DO 85 I=1,NRUF
                  RUF(3,I)=SRD*VSLOPE+RUF(3,I)
 85             CONTINUE
              ENDIF
            ELSE
              IF(NSA.EQ.NRUF) THEN
                DO 60 I=1,NRUF
                  RUF(1,I)=FLOAT(I)
                  RUF(3,I)=YMIN
 60             CONTINUE
              ELSE IF(NRUF.EQ.NDEP) THEN
                DO 65 I=1,NRUF/2
                  RUF(1,(I*2-1))=FLOAT(I)
                  RUF(1,(I*2))=FLOAT(I)
 65             CONTINUE
              ENDIF
            ENDIF
            IF(SAFG.EQ.1) THEN
              NSA=NSA+1
              XSA(NSA)=0.0
            ENDIF
           IF(TMPFLG.EQ.0) THEN
C
              DO 66 I=1,NRUF
                SIN(1,I)=RUF(1,I)
                SIN(2,I)=1.0
 66           CONTINUE
              ONRUF=NRUF
          ENDIF
            DFG=0
            SAFG=0
            GRFG=0
            NFG=0
          ENDIF
          IF(CODE.EQ.'XT '.OR.CODE.EQ.'AS '.OR.CODE.EQ.'XS ') THEN
            SECID=BLKOUT
            SECID(1:5)=CBUF(3:7)
            MAX=6
            NCHAR=70
            DO 3 I=1,MAX
              VAR(I)=0.0
 3          CONTINUE
            IF(CODE.NE.'XT') THEN
              TMPFLG=0
              VAR(1)=SRD
              VAR(5)=VSLOPE
              CHRDUM(1:NCHAR)=CBUF(8:77)
              CALL PCHRR(CHRDUM,NCHAR,MAX,NCNT,VAR)
              SRD=VAR(1)
              IF(VAR(2).GT.0.0)THEN
                SKEW=3.14159*VAR(2)/180.0
                SKEW=COS(SKEW)
              ELSE
                SKEW=1.0
              ENDIF 
              VSLOPE=VAR(5)
              CORDAT=VAR(6)
            ELSE
              TMPFLG=1
              VAR(1)=SRD
              VAR(2)=VSLOPE
              CHRDUM(1:NCHAR)=CBUF(8:77)
              CALL PCHRR(CHRDUM,NCHAR,MAX,NCNT,VAR)
              SRD=VAR(1)
              VSLOPE=VAR(2)
            ENDIF
          ENDIF
          FLG=1
      ELSE IF(CODE.EQ.'T1 '.OR.CODE.EQ.'T2 '.OR.CODE.EQ.'T3 '.OR.
     #        CODE.EQ.COM.OR.CODE.EQ.'Q  '.OR.CODE.EQ.'WS ') THEN
        FLG=1
      ELSE IF(CODE.EQ.'*PA'.OR.CODE.EQ.'*PD') THEN
        FLG=1
      ENDIF
C
        IF(FLG.EQ.1.AND.RREC.LE.EREC) GO TO 15
C
        IF(CODE.EQ.'*ID')THEN
          SECID(1:16)=CBUF(2:16)
        ELSE
C       * Reads N,ND,SA,GT,GR,and *SN,*SD cards into arrays *
        MAX=35
        NCHAR=76
        DO 4 I=1,MAX
          VAR(I)=0.0
 4      CONTINUE
        CHRDUM(1:NCHAR)=CBUF(2:77)
        CALL PCHRR(CHRDUM,NCHAR,MAX,NCNT,VAR)
        IF(CODE.EQ.'GR ') THEN
          IF(GRFG.EQ.0) N=0
           GRFG=1
          IO=N
          N=N+(NCNT/2)
          IF(N.GT.NCORD)THEN
            ERRNO=2
            CALL ERRPRT(ERRIO,ERRNO,NCORD,SECID)
            EDFG=1
          ELSE
            DO 18 I=1,NCNT/2
              S(IO+I)=VAR(2*I-1)*SKEW
              G(IO+I)=VAR(2*I) + CORDAT
 18         CONTINUE
            YMIN=IXMIN(G,N)
          ENDIF
        ELSE IF(CODE.EQ.'N  ') THEN
          IF(NFG.EQ.0) THEN
            ONRUF=NRUF
            NRUF=0
          ENDIF
          NFG=1
          IO=NRUF
          NRUF=NRUF+NCNT
          IF(NRUF.GT.NSBS)THEN
            ERRNO=3
            CALL ERRPRT(ERRIO,ERRNO,NSBS,SECID)
            EDFG=1
          ELSE
            DO 20 I=1,NCNT
              RUF(2,IO+I)=VAR(I)
 20         CONTINUE
          ENDIF
        ELSE IF(CODE.EQ.'ND ') THEN
          IF(DFG.EQ.0) NDEP=0
          DFG=1
          IO=NDEP
          NDEP=NDEP+NCNT
          IF(NDEP.GT.NSBS)THEN
            ERRNO=4
            CALL ERRPRT(ERRIO,ERRNO,NSBS,SECID)
            EDFG=1
          ELSE
            DO 25 I=1,NCNT
              RUF(3,IO+I)=VAR(I)
 25         CONTINUE
          ENDIF
        ELSE IF(CODE.EQ.'SA ') THEN
          IF(SAFG.EQ.0) THEN
            ONSA=NSA
            NSA=0
          ENDIF
          SAFG=1
          IO=NSA
          NSA=NSA+NCNT
          IF(NSA.GT.NSUBS)THEN
            ERRNO=5
            CALL ERRPRT(ERRIO,ERRNO,NSUBS,SECID)
            EDFG=1
          ELSE
            IF(NSA.EQ.0) XSA(1)=0.0
            DO 30 IJ=1,NCNT
              XSA(IO+IJ)=VAR(IJ)*SKEW
 30         CONTINUE
          ENDIF
        ELSE IF(CODE.EQ.'GT ') THEN
          IF(VAR(4).EQ.0.0) VAR(4)=1.0
          DO 50 I=1,N
            S(I)=(S(I)-VAR(5))*VAR(4)+VAR(5)
            G(I)=G(I)+VAR(1)
 50       CONTINUE
          IF(VAR(2).NE.0.0.OR.VAR(3).NE.0.0) THEN
            I=0
            J=0
 55         CONTINUE
            I=I+1
            IF(S(I).GE.VAR(2).AND.S(I).LE.VAR(3)) THEN
              J=J+1
              S(J)=S(I)
              G(J)=G(I)
            ENDIF
            IF(I.LT.N) GO TO 55
            N=J
          ENDIF
        ELSE IF(CODE.EQ.SINJNK) THEN
          IF(SINFG.EQ.0) NSIN=0
          SINFG=1
          IO=NSIN
          NSIN=NSIN+NCNT
          IF(NSIN.GT.NSBS)THEN
            ERRNO=6
            CALL ERRPRT(ERRIO,ERRNO,NSBS,SECID)
            EDFG=1
          ELSE
            DO 40 I=1,NCNT
              SIN(2,IO+I)=VAR(I)
 40         CONTINUE
          ENDIF
        ELSE IF(CODE.EQ.'*SD')THEN
          IF(DSFG.EQ.0) NSDEP=0
          DSFG=1
          IO=NSDEP
          NSDEP=NSDEP+NCNT
          IF(NSDEP.GT.NSBS)THEN
            ERRNO=7
            CALL ERRPRT(ERRIO,ERRNO,NSBS,SECID)
            EDFG=1
          ELSE
            DO 45 I=1,NCNT
              SIN(3,IO+I)=VAR(I)
 45         CONTINUE
          ENDIF
        ENDIF
        ENDIF
        IF(RREC.LE.EREC) GO TO 15
        IF(NFG.EQ.1)THEN
          IF(SAFG.EQ.1)THEN
            NSA=NSA+1
          ELSE IF(NSA.LE.0.OR.SAFG.NE.1)THEN
            NSA=1
          ENDIF
          ODFG=EDFG
          IF(NDEP.NE.0)THEN
            IF(NDEP.NE.NRUF)THEN
              ERRNO=10
              CALL ERRPRT(ERRIO,ERRNO,IOVER,SECID)
              EDFG=1
            ENDIF
            IF(NRUF.NE.(2*NSA))THEN
              ERRNO=8
              CALL ERRPRT(ERRIO,ERRNO,IOVER,SECID)
              EDFG=1
            ENDIF
          ELSE IF(NRUF.NE.NSA)THEN
            ERRNO=8
            CALL ERRPRT(ERRIO,ERRNO,IOVER,SECID)
            EDFG=1
          ENDIF
          IF(ODFG.EQ.EDFG) CALL FXARY(NSA,NRUF,RUF)
        ENDIF
        IF(SINFG.EQ.1.AND.NSIN.GT.0) THEN
        ODFG=EDFG
          IF(NSDEP.NE.0)THEN
            IF(NSDEP.NE.NSIN)THEN
              ERRNO=11
              CALL ERRPRT(ERRIO,ERRNO,IOVER,SECID)
              EDFG=1
            ENDIF
            IF(NSIN.NE.(2*NSA))THEN
              ERRNO=9
              CALL ERRPRT(ERRIO,ERRNO,IOVER,SECID)
              EDFG=1
            ENDIF
          ELSE IF(NSIN.NE.NSA)THEN
            ERRNO=9
            CALL ERRPRT(ERRIO,ERRNO,IOVER,SECID)
            EDFG=1
          ENDIF
          IF(ODFG.EQ.EDFG) CALL FXARY(NSA,NSIN,SIN)
        ENDIF
        IF(N.LE.0) THEN
          ERRNO=17
          CALL ERRPRT(ERRIO,ERRNO,IOVER,SECID)
          EDFG=2
        ELSE IF(NRUF.LE.0) THEN
          ERRNO=18
          CALL ERRPRT(ERRIO,ERRNO,IOVER,SECID)
          EDFG=2
        ENDIF
C
        IF(SI.EQ.1.OR.SI.EQ.3)THEN
C         convert meters to feet
          call m_to_ft_array(N,S)
          call m_to_ft_array(N,G)
          call m_to_ft_array(NSA,XSA)

          SRD = m_to_ft(SRD)
        ENDIF
C
        RETURN
        END
C
C
      SUBROUTINE CDOUT
     I                (FTEMP,IREC,N,VAL,CODE)
C
C     + + + PURPOSE + + +
C     Writes WSPRO card images to temporary DA file.
C
C     + + +  ARGUMENT DEFINITIONS + + +
C     FTEMP  - file unit on which temp DA file is openned
C     IREC   - starting record at which xsprop file it to be places at.
C     N      - no. of real values to output
C     VAL    - array of real values
C     CODE   - card type identifier
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER N,IREC,FTEMP,SCOL,LEFT
      REAL VAL(N)
      CHARACTER*3 CODE
C
C     + + + LOCAL VARIABLES + + +
      INTEGER MAX,NCOL,I
      CHARACTER*19 PNT
C
C
      IF(CODE.EQ.'N  '.OR.CODE.EQ.'*SN')THEN
        MAX=10
        PNT='(A3,7X,10(F6.4,1X))'
      ELSE
        MAX=6
        PNT='(A3,7X,6(F10.4,1X))'
      ENDIF
      LEFT=N
      SCOL=1
      NCOL=MAX
10    CONTINUE
        IREC=IREC+1
        IF(LEFT.LT.MAX) NCOL=NCOL-MAX+LEFT
        WRITE(FTEMP,FMT=PNT,REC=IREC)CODE,(VAL(I),I=SCOL,NCOL)
        LEFT=LEFT-MAX
        SCOL=NCOL+1
        NCOL=NCOL+MAX
      IF(LEFT.GT.0) GOTO 10
C
      RETURN
      END
C
C
      SUBROUTINE  VREADV
     I                  (LUNIT,X,NXMAX,NIN)
C
C     + + + PURPOSE + + +
C     READS REAL DATA IN FORTRAN FREE (LIST-DIRECTED) FORMAT
C     INTO VECTOR X  AND COUNTS THE NUMBER OF ITEMS
C     ACTUALLY READ IN.   THE INPUT MUST BE IN FORTRAN LIST-DIRECTED
C     FORMAT AND MUST --  YES,   M U S T   -- BE TERMINATED BY THE END-OF-
C     LIST MARKER (/).   ALSO, ALL INPUT VALUES MUST BE GREATER THAN
C     -1.E+38 ('MINUS INFINITY').
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER LUNIT,NIN,NXMAX
      REAL X(NXMAX)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     LUNIT  - LOGICAL UNIT NUMBER FOR INPUT
C     X      - REAL VECTOR TO BE FILLED WITH VALUES READ FROM UNIT LUNIT
C     NXMAX  - MAX NUMBER OF DATA ITEMS THAT X CAN HOLD
C     NIN    - NUMBER OF ITEMS ACTUALLY READ
C
C     NOTE --  ALL NXMAX ITEMS OF X FIRST ARE SET TO MINUS INFINITY, THEN
C     ARE RESET BY READING FROM THE FILE.  NIN IS SET BY COUNTING THE
C     X-VALUES UP TO (BUT NOT INCLUDING) THE FIRST MINUS INFINITY VALUE.
C     EXAMPLE -- THE INPUT CARD
C                            123  456   ,,  101 112 /
C     WOULD YIELD X =  123., 456., -1E38, 101., 112.,  -1E38, -1E38,....
C     AND WOULD YIELD NIN = 2.
C
C     WKIRBY,SWB
C
C     + + + LOCAL VARIABLES + + +
      INTEGER I
      REAL  XNULL,TAG
C
C     + + + INITIALIZATIONS + + +
      DATA     XNULL / -1.1E38 /
C
C
      DO 10 I=1,NXMAX
        X(I) = 1.01*XNULL
 10   CONTINUE
      TAG = 1.01*XNULL
C
      READ(LUNIT,*,END=20) X,TAG
   20 CONTINUE
      NIN=0
 80   CONTINUE
      NIN=NIN+1
      IF(X(NIN).GT.XNULL.AND.NIN.LT.NXMAX) GO TO 80
      IF(X(NIN).LE.XNULL) THEN
        NIN=NIN-1
      ELSE
        NIN=NXMAX
        IF(TAG.GT.XNULL) THEN
          I=NIN+1
          WRITE(*,*)'*** VREADV EXCESS DATA IGNORED.  ITEM NO,
     #                 ITEM=',I,TAG
        ENDIF
      ENDIF
C
      RETURN
      END
C
C
C
        SUBROUTINE PCHRR
     I                 (CBUF,NCHAR,MAX,
     O                  NCNT,VAR)
C
C     + + + PURPOSE + + +
C     Parses a character array that has reals seperated by commas or
C     blanks into a real array.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER MAX,NCHAR,NCNT
      REAL VAR(MAX)
      CHARACTER(LEN=*) CBUF
      CHARACTER DUMCHR*20
      CHARACTER*1 ONCHR,OLDC
C
C     + + + EXTERNALS + + +
      EXTERNAL CHRREL
C
C     + + + ARGUMENT DEFINITIONS + + +
C     CBUF - character array to be parsed
C     NCHAR- no. of characters in CBUF array
C     MAX  - max size of out putted real array
C     NCNT - actual no. of reals put to real array VAR
C     VAR  - real array containing data from CBUF
C
C     + + + LOCAL VARIABLES + + +
      INTEGER I,BGN,ILEN,EFG,ERCNT
C
C
      EFG=0
      ERCNT=0
      NCNT=0
      BGN=1
      I=0
      OLDC = ','
 10   CONTINUE
      DUMCHR='                    '
      I=I+1
      ONCHR=CBUF(I:I)
      IF(ONCHR.EQ.','.OR.ONCHR.EQ. ' '.OR.ONCHR.EQ.'*') THEN
        ILEN=I-BGN
        IF(ONCHR.EQ.'*'.OR.ILEN.GT.0) NCNT=NCNT+1
        IF(ONCHR.EQ.','.AND.ONCHR.EQ.OLDC) NCNT=NCNT+1
        IF(ILEN.GT.0) THEN
          DUMCHR(1:ILEN)=CBUF(BGN:(I-1))
          CALL CHRREL(DUMCHR,VAR(NCNT),EFG)
        ENDIF
        IF(EFG.NE.0) ERCNT=ERCNT+1
        BGN=I+1
      ELSE IF(I.EQ.NCHAR.AND.ONCHR.NE.' ') THEN
        ILEN=I-BGN
        IF(ONCHR.NE.','.AND.ONCHR.NE.'*'.AND.ILEN.GT.0) NCNT=NCNT+1
        IF(ILEN.GT.0) THEN
          DUMCHR(1:ILEN)=CBUF(BGN:I)
          CALL CHRREL(DUMCHR,VAR(NCNT),EFG)
        ENDIF
        IF(EFG.NE.0) ERCNT=ERCNT+1
      ENDIF
      OLDC=ONCHR
      IF(I.LT.NCHAR.AND.NCNT.LT.MAX) GO TO 10
C
      RETURN
      END
C
      SUBROUTINE CHRREL(STRING,VALUE,IERR)
C
C---- CHAR2R ----------------------------------------------------------S
C
C  PURPOSE:
C    CONVERTS A CHARACTER REPRESENTAION OF AN REAL NUMBER TO ITS
C    NUMBERIC VALUE.
C
C  INPUT ARGUMENT(S):
C    STRING - CHARACTER REPRESENTATION OF THE INTEGER.
C
C  OUTPUT ARGUMENT(S):
C    VALUE  - NUMERIC VALUE OF THE REAL NUMBER REPRESENTED BY STRING.
C    ERR    - LOGICAL ERROR CODE SET TRUE IF AN INVALID CHARACTER IS
C             ENCOUNTERED IN THE STRING.
C
C----------------------------------------------------------------------F
C
C---- Declarations (arguments)
      INTEGER IERR
      REAL      VALUE
      CHARACTER*(*) STRING
C
C---- Declarations (local values)
      INTEGER   DIGIT,MINUS,PLUS,SPACE,ZERO,NINE,POINT,I,IHI
      REAL      FACTOR
      LOGICAL   DECPT
C
C-----ASSIGN VALUES
      VALUE=0.
      IERR=0
      DECPT=.FALSE.
      SIGN=1.
      FACTOR=0.1
      MINUS=ICHAR('-')
      POINT=ICHAR('.')
      PLUS=ICHAR('+')
      SPACE=ICHAR(' ')
      ZERO=ICHAR('0')
      NINE=ICHAR('9')
      IHI=LEN(STRING)
      DO 10 I=1,IHI
        IF(ICHAR(STRING(I:I)).GT.SPACE) GOTO 20
10    CONTINUE
      IERR=1
      GOTO 999
20    DIGIT=ICHAR(STRING(I:I))
C
C-----DETERMINE SIGN
      IF(DIGIT.EQ.PLUS) THEN
        SIGN=1.
      ELSEIF(DIGIT.EQ.POINT) THEN
        SIGN=1.
        DECPT=.TRUE.
      ELSEIF(DIGIT.EQ.MINUS) THEN
        SIGN=-1.
      ELSEIF(DIGIT.GE.ZERO .AND. DIGIT.LE.NINE) THEN
        VALUE=REAL(DIGIT-ZERO)
      ELSE
        IERR=1
        GOTO 999
      ENDIF
C
C-----CONVERT CHARACTERS TO DIGITS AND SUM
30    I=I+1
      DIGIT=ICHAR(STRING(I:I))
      IF(DIGIT.EQ.SPACE) GOTO 40
      IF(DIGIT.EQ.POINT) THEN
        IF(DECPT) THEN
          IERR=1
          GOTO 999
        ELSE
          DECPT=.TRUE.
        ENDIF
      ELSEIF(DIGIT.GE.ZERO .AND. DIGIT.LE.NINE) THEN
        IF(DECPT) THEN
          VALUE=VALUE+FACTOR*REAL(DIGIT-ZERO)
          FACTOR=0.1*FACTOR
        ELSE
          VALUE=10.*VALUE+REAL(DIGIT-ZERO)
        ENDIF
      ELSE
        IERR=1
        GOTO 999
      ENDIF
      GOTO 30
40    CONTINUE
      VALUE=SIGN*VALUE
999   RETURN
      END
C
      SUBROUTINE FXARY
     I                (NSBA,NRUF,
     M                 RUF)
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER NSBA,NRUF
      REAL RUF(3,NRUF)
C
C     + + + ARGUMENT DEFINITIONS + + +
C     NSBA   - NO. OF SUB AREAS
C     NRUF   - SIZE OF ROUGHNESS ARRAY
C     RUF    - (i,k) i=1, subarea index i=2 roughness values  i=3 depth
C               k = 1 to NRUF
C
C     + + + LOCAL VARIABLES + + +
      INTEGER I,K
C
C     + + + INTRINSICS + + +
      INTRINSIC FLOAT
C
C
      K=NSBA
      IF(K.EQ.NRUF) THEN
        DO 10 I=1,NRUF
          RUF(1,I)=FLOAT(I)
          RUF(3,I)=0.0
 10     CONTINUE
      ELSE IF(K.LT.NRUF) THEN
        DO 15 I=1,K
          RUF(1,I*2)=FLOAT(I)
          RUF(1,I*2-1)=FLOAT(I)
 15     CONTINUE
      ENDIF
C
      RETURN
      END
C
C
      SUBROUTINE DACOMP
     I                (FTEMP,
     O                 NCOMP,COMP,ISEC,SECPRP,ELID,NCARDS,NHDR,TBTYP,
     O                 XSREC,NOUT,OPOUT)
C
C     + + + PURPOSE + + +
C     Reads WSPRO file into a direct access temporary file and initializes
C     the computational arrays.
C
C     + + + PARAMETERS + + +
      INTEGER NXSEC
      PARAMETER (NXSEC=200)
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER FTEMP(2),NCOMP,ISEC,NCARDS,NHDR,TBTYP(3),NOUT,
     #        XSREC(NXSEC),OPOUT(17)
      REAL COMP(6,5),SECPRP(3,NXSEC)
      CHARACTER*16 ELID(NXSEC)
C
C     + + + LOCAL VARIABLES + + +
      INTEGER NCNT,NCHAR,MAX,IREC,ERRIO
      REAL VAR(5)
      CHARACTER*75 CBUF
      CHARACTER*3 CODE
      CHARACTER*1 OPT
      CHARACTER*16 BLKOUT
C
C     + + + ARGUMENT DEFINITIONS + + +
C     FCARD   - unit on which WSPRO file is openned
C     FTEMP   - unit on which temp DA is openned
C     NCOMP   - no. of general compation cards read
C     COMP    - arra y of min, max, geometric ratio and increment sizes
C               for determing values at which hydraulic properties will
C               be computed
C     ISEC    - no. of cards for determining stages at which hydraulic
C               properties will be computed.
C     ELID    - section I.D.
C     NCARDS  - no. of cards read from WSPRO file
C     NHDR    - no. of cross sections read
C
C     + + + EXTERNALS + + +
      EXTERNAL SETOUT,PCHRR
C
C     + + + FORMATS + + +
 100  FORMAT(A3,1X,A1,A75)
C
C
      ERRIO=FTEMP(2)
      MAX=5
      BLKOUT='                '
      IREC=1
      READ(FTEMP(1),'(I4)',REC=IREC)NCARDS
 5    CONTINUE
      IREC=IREC+1
      READ(FTEMP(1),100,REC=IREC) CODE,OPT,CBUF
      NCHAR=75
      IF(CODE(1:1).EQ.'*')THEN
        CALL SETOUT(CODE,OPT,CBUF,NCHAR,ERRIO,TBTYP,NCOMP,
     #              COMP,NOUT,OPOUT)
      ELSE IF(CODE.EQ.'SI ')THEN
        SI=VAR(1)
      ELSE IF(CODE.EQ.'HP ')THEN
C     * found WSPRO hydraulic prop card *
        ISEC=ISEC+1
        ELID(ISEC)=CBUF(1:5)//BLKOUT(1:11)
        CALL PCHRR(CBUF(6:75),NCHAR,MAX,NCNT,VAR)
        SECPRP(1,ISEC)=VAR(1)
        SECPRP(2,ISEC)=VAR(2)
        SECPRP(3,ISEC)=VAR(3)
      ELSE
        IF(CODE.EQ.'XS '.OR.CODE.EQ.'XT '.OR.CODE.EQ.'AS '.OR.CODE.EQ.
     #     'BR '.OR.CODE.EQ.'CV '.OR.CODE.EQ.'SD '.OR.CODE.EQ.'XR ')
     #     THEN
          NHDR=NHDR+1
          XSREC(NHDR)=IREC
        ENDIF
      ENDIF
      IF(IREC.LT.NCARDS) GO TO 5
      IF(SI.EQ.1.OR.SI.EQ.3)THEN
        DO 10 I=1,ISEC
          SECPRP(1,ISEC)=SECPRP(1,ISEC)*(30.48/100.)
          SECPRP(2,ISEC)=SECPRP(2,ISEC)*(30.48/100.)
          SECPRP(3,ISEC)=SECPRP(3,ISEC)*(30.48/100.)
 10    CONTINUE
      ENDIF
C
C 998  CONTINUE
C
      RETURN
      END
C
C
      SUBROUTINE ERRPRT
     I                 (ERRIO,ERRNO,IOVER,SECID)
C
C     + + + PURPOSE + + +
C     Routine writes error messages to the IO unit specified
C     by ERRIO.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER ERRIO,ERRNO,IOVER
      CHARACTER*16 SECID
C
C     + + + ARGUMENT DEFINITIONS + + +
C     ERRIO   - IO unit specified for error messages
C     ERRNO   - number of error string to write to ERRIO
C     IOVER   - integer value included in error message
C     SECID   - section ID at which error occurred.
C
C     + + + LOCAL VARIABLES + + +
      INTEGER IO
      CHARACTER*57 ERRSTG(21)
C
C     + + + INITIALIZATIONS + + +
      DATA (ERRSTG(I), I=1,15) /
     # ' allowable computation records, EXCEEDED                 ',
     # ' allowable x,y  coordinates,  ---------- EXCEEDED for ID ',
     # ' allowable roughness values, ----------- EXCEEDED for ID ',
     # ' allowable roughness depths,  ---------- EXCEEDED for ID ',
     # ' subarea breakpoints allowed.  --------- EXCEEDED for ID ',
     # ' sinuosity values allowed.  ------------ EXCEEDED for ID ',
     # ' sinuosity depths allowed.  ------------ EXCEEDED for ID ',
     # ' Inconsistent no. of roughnesses  and  subareas  for  ID ',
     # ' Inconsistent no. of sinuosities  and  subareas  for  ID ',
     # ' Inconsistent no. of roughnesses & roughness depths @ ID ',
     # ' Inconsistent no. of sinuosities & sinousity depths @ ID ',
     # ' allowable number of cross sections, EXCEEDED            ',
     # ' No ID found in input data file that matches wanted - ID ',
     # ' x,y coordinates are ordered funny ------------------ ID ',
     # ' area computation fails set error tolerance of 0.005- ID '/
      DATA (ERRSTG(I), I=16,21) /
     # ' Branch no., no computations specified -------------- ID ',
     # ' no x, y coordinates entered  ----------------------- ID ',
     # ' no roughness values entered  ----------------------- ID ',
     # ' no HP record entered for cross section ------------- ID ',
     # ' range on *PD swapped to ascending order-------------    ',
     # ' RATIO on *PD set to default of 1--------------------    '
     #/
C
C     + + + FORMATS + + +
 100  FORMAT(1X,I3,A57,A16)
C
      IO=ERRIO
      IF(ERRIO.LT.0) THEN
        IO=ABS(ERRIO)
        WRITE(*,100) IOVER,ERRSTG(ERRNO),SECID
      ENDIF
      WRITE(IO,100) IOVER,ERRSTG(ERRNO),SECID
C
      RETURN
      END
C
C
      SUBROUTINE PROPER
     I                 (S,G,N,NSA,XSA,YS,NRUF,RUF,SIN,
     O                  POUT,SUBPRP,KXTEND)
C
C     + + + PURPOSE + + +
C     Computes the hydraulic properties at a cross section given
C     the x,y pairs describing the geometry and the roughness parameters.
C
C     * * * DUMMY ARGUMENTS * * *
      INTEGER N,NSA,NRUF,KXTEND
      REAL RUF(3,NRUF),SIN(3,NRUF),YS,POUT(17),S(N),G(N),
     #     XSA(NSA),SUBPRP(7,NSA)
C
C     * * * ARGUMENT DEFINITIONS * * *
C     NSA    - number of subarea breakpts. NSA+1= no. of subsections
C     ROUGHN - (i,NSA)roughness array, i=1 depth D1 i=2 roughness N1
C                     i=3 depth D2 i=4 roughness N2
C     SUBPRP1WIDTH  - subsection widths
C     SUBPRP2AREAV  - subsection areas
C     SUBPRP3PERIMV - subsection wetted perimeters
C     SUBPRP4ISTMOM - first moment vertical coordinate
C     YS     - stage
C     POUT   - array containing hydraulic properties for given inputs,
C              1- total conveyance
C              2-total width
C              3 - total area
C              4 - phim
C              5 - phiv2
C              6 - total wetted perimeter
C              7 - area weighted sinuosity
C              8 - conveyance weighted sinuosity
C              9 - alpha
C              10 - beta
C              11 - composite roughness
C              12 - y centriod of the channel section
C              13 - stage
C              14 - depth (stage - channel bottom elevation)
C              15 - total sinuosity weighted convenyance
C              16 - left edge of water el.
C              17 - right edge of water el.
C
C     * * * LOCAL VARIABLES * * *
      INTEGER ISA,I,INDS,IND,IXMIN,K
      REAL Q,A,P,WID,DEP,RUFN,UROUGH,V
     #     ,TINY,A1486,SINN,USIN,R,REL,LEL
C
C     + + + INTRINSICS + + +
      INTRINSIC AMAX1,SQRT
C
C     + + + EXTERNAL + + +
      EXTERNAL RAMP,XSGEOM,IXMIN
C
C     + + + INITIALIZATIONS + + +
      DATA TINY, A1486 / 1E-9, 1.49 /
C
C
      CALL XSGEOM(S,G,N,XSA,NSA,YS,SUBPRP,LEL,REL,KXTEND)
      DO 10 I=1,17
        POUT(I)=0.
  10  CONTINUE
      POUT(9)=1.
      POUT(10)=1.
      POUT(16)=LEL
      POUT(17)=REL
C
      INDS=1
      IND=1
      POUT(13)=YS
      POUT(14)=YS-G(IXMIN(G,N))
      DO 20 ISA=1,NSA
        Q=0.
        A=SUBPRP(2,ISA)
        IF(POUT(14).EQ.0.OR.A.GT.0) THEN
          P=AMAX1(TINY,SUBPRP(3,ISA))
          POUT(2)=POUT(2)+SUBPRP(1,ISA)
          WID=SUBPRP(1,ISA)
          POUT(6)=POUT(6)+P
        ENDIF
        IF(A.GT.0.) THEN
          DEP=A/(AMAX1(TINY,WID))
          K=ISA
          CALL RAMP(DEP,NRUF,RUF,K,IND,RUFN)
          UROUGH=AMAX1(TINY,RUFN)
          V=A1486*((A/P)**.66666667)/UROUGH
          Q=V*A
          SUBPRP(7,ISA)=UROUGH
          POUT(1)=POUT(1)+Q
          POUT(3)=POUT(3)+A
          POUT(4)=POUT(4)+Q*V
          POUT(5)=POUT(5)+Q*V**2
C
C         sinuosity code
          CALL RAMP(DEP,NRUF,SIN,K,INDS,SINN)
          USIN=AMAX1(TINY,SINN)
          IF(USIN.GT.TINY) THEN
          POUT(7)=POUT(7)+USIN*A
          POUT(8)=POUT(8)+Q*SQRT(USIN)
          ENDIF
          POUT(12)=POUT(12)+SUBPRP(4,ISA)
          POUT(15)=POUT(15)+Q/(SQRT(USIN))
        ENDIF
        SUBPRP(6,ISA)=Q
20    CONTINUE
C
      IF(POUT(1).GT.0) THEN
        R=POUT(3)/POUT(1)
        POUT(7)=POUT(7)/POUT(3)
        POUT(8)=POUT(8)/POUT(1)
        POUT(10)=POUT(4)*R/POUT(1)
        POUT(9)=POUT(5)*R**2/POUT(1)
        POUT(11)=A1486*POUT(3)*((POUT(3)/POUT(6))**.666667)/POUT(1)
         POUT(12)=POUT(12)/POUT(3)
      ENDIF
C
C
      RETURN
      END
C
C
C
C
       SUBROUTINE STAGE
     I                (S,G,N,NSA,XSA,AREA,NRUF,RUF,SIN,
     O                 POUT,SUBPRP,KXTEND,ERROR)
C
C     + + + PURPOSE + + +
C     Given the areas of the channel cross section it computes the
C     stage and hydraulic properties.
C
C     * * * DUMMY ARGUMENTS * * *
      INTEGER N,NSA,KXTEND,NRUF
      REAL S(N),G(N),XSA(NSA),AREA,RUF(3,NRUF),SIN(3,NRUF)
     #     ,POUT(17),SUBPRP(7,NSA)
C
C     * * * ARGUMENT DEFINITIONS * * *
C
C          S - station(distance)
C          G - ground elevation measured from a datum
C          N - number of station, ground elevation pairs
C        XSA - subarea breakpoints
C        NSA - number of subarea breakpoints
C       AREA - inputted total area
C       POUT - array containing hydraulic properties for given inputs.
C              1 - total convenyance
C              2 - total width
C              3 - total area
C              4 - phim
C              5 - phiv2
C              6 - total wetted perimeter
C              7 - average sinuosity
C              8 - weighted sinuosity
C              9 - alpha
C              10 - beta
C              11 - composite roughness
C              12 -  y centroid
C              13 -  stage
C              14 - depth (stage - channel bottom elevation)
C              15 - total sinuosity weighted convenyance
C              16 - left edge of water el.
C              17 - right edge of water el.
C     KXTEND - channel sides extended (bank overflow)
C     ERROR  - error flag, 1- funny coordinates, 2- solution not closed
C
C     *  *  * PARAMETERS  *   *  *
      INTEGER NCORD
      PARAMETER(NCORD=150)
C
C     * * * LOCAL VARIABLES * * *
      INTEGER J(10),K(10),I,I2,DONE,IN,ERROR,IBOTTM,IXMIN
      INTEGER NCHN,JJ,KK,OCHN
      REAL A1,AN,A0,AS,YMAX,XD(NCORD),YD(NCORD),PERR
     #     ,AMAX,YS,Y0,LOWER,HIGH
C
C     + + + EXTERNALS + + +
      EXTERNAL IXMIN,COEFFI,PROPER
C
C     + + + INTRINSICS + + +
      INTRINSIC MAX
C
C
      NCHN=1
      ERROR=0
      PERR=0.0
      IF(AREA.LE.0.0) THEN
        YS=G(IXMIN(G,N))
        ERROR=0
      ELSE
C     left bank
      CALL PROPER (S,G,N,NSA,XSA,G(1),NRUF,RUF,SIN,POUT,SUBPRP,KXTEND)
      A0=POUT(3)
C     right bank
      CALL PROPER (S,G,N,NSA,XSA,G(N),NRUF,RUF,SIN,POUT,SUBPRP,KXTEND)
      AN=POUT(3)
C     assignment below added to ensure that A1 is initialized when IF statement is false
      A1=A0
      IF(AREA.LE.A0.AND.AREA.LE.AN) THEN
C       no banks need to be extended
        DONE=0
        IBOTTM=IXMIN(G,N)
        I=1
 20     CONTINUE
          I=I+1
          A1=A0
          CALL PROPER (S,G,N,NSA,XSA,G(I),NRUF,RUF,SIN,POUT,SUBPRP,
     #                 KXTEND)
          A0=POUT(3)
          IF(A1.GE.AREA.AND.A0.LE.AREA) THEN
            J(1)=I-1
            DONE=1
          ENDIF
        IF(DONE.EQ.0.AND.I.LE.IBOTTM) GOTO 20
        DONE=0
        I2=N
 30     CONTINUE
          I2=I2-1
          A1=AN
          CALL PROPER(S,G,N,NSA,XSA,G(I2),NRUF,RUF,SIN,POUT,SUBPRP,
     #                KXTEND)
          AN=POUT(3)
          IF(A1.GE.AREA.AND.AN.LE.AREA) THEN
            K(1)=I2+1
            DONE=1
          ENDIF
        IF(DONE.EQ.0.AND.I2.GE.IBOTTM) GOTO 30
        IF(J(1).LT.K(1)) THEN
          IF(G(J(1)+1).GT.G(K(1)-1)) THEN
            Y0=G(J(1)+1)
          ELSE
            A0=AN
            Y0=G(K(1)-1)
          ENDIF
          CALL COEFFI (J,K,NCHN,Y0,A0,S,G,K(1),AREA,YS)
        ELSE
          ERROR=1
          YS=G(IBOTTM)
          AREA=0.0
        ENDIF
      ELSE IF(AREA.GE.A1.AND.AREA.GE.AN)THEN
C       both sides of channel need to be extended, donot solve quadratic
        YMAX=MAX(G(1),G(N))
        AMAX=MAX(A1,AN)
        YS=(AREA-AMAX)/(S(N)-S(1))+YMAX
        J(1)=1
        K(1)=N
      ELSE
C       one side of channel needed to be extended. expand coordinate arrays
C       and solve quadratic
        IF(AREA.LT.A1.AND.AREA.GE.AN) THEN
          DO 40 IN=1,N
            XD(IN)=S(IN)
            YD(IN)=G(IN)
  40      CONTINUE
          XD(N+1)=S(N)
          YD(N+1)=G(1)
          A0=AN
          Y0=G(N)
          J(1)=1
          K(1)=N+1
        ELSE IF(AREA.LT.AN.AND.AREA.GE.A1) THEN
          XD(1)=S(1)
          YD(1)=G(N)
          DO 50 IN=1,N
            XD(IN+1)=S(IN)
            YD(IN+1)=G(IN)
   50     CONTINUE
          A0=A1
          Y0=G(1)
          J(1)=1
          K(1)=N+1
        ENDIF
        CALL COEFFI(J,K,NCHN,Y0,A0,XD,YD,K(1),AREA,YS)
      ENDIF
      ENDIF
CC
      NCHN=1
 70   CONTINUE
      OCHN=NCHN
      IF(PERR.GT.0.005) THEN
C       multiple channels
        KK=K(NCHN)
        JJ=J(NCHN)
        IF(G(JJ).LT.G(KK)) THEN
          HIGH=G(JJ)
        ELSE
          HIGH=G(KK)
        ENDIF
        IF(G(JJ+1).GT.G(KK-1)) THEN
          LOWER=G(J(NCHN)+1)
        ELSE
          LOWER=G(K(NCHN)-1)
        ENDIF
C       Search for pair above lower limits
        I=J(NCHN)+1
 15   CONTINUE
      I=I+1
      DONE=0
      IF(G(I).GT.LOWER) THEN
        CALL PROPER(S,G,N,NSA,XSA,G(I),NRUF,RUF,SIN,POUT,SUBPRP,
     #              KXTEND)
        IF(POUT(3).LT.AREA) THEN
C         new highest lower
          Y0=G(I)
          LOWER=Y0
          A0=POUT(3)
        ELSE IF(POUT(3).GE.AREA) THEN
C         new channel, left bank found
          K(NCHN+1)=K(NCHN)
          K(NCHN)=I
          IF(G(I-1).GT.LOWER) THEN
            LOWER=G(I-1)
            Y0=LOWER
            A0=POUT(3)
          ENDIF
          IF(G(I).LT.HIGH) THEN
            HIGH=G(I)
          ENDIF
10        CONTINUE
          IF(G(I).LT.HIGH) THEN
            CALL PROPER(S,G,N,NSA,XSA,G(I),NRUF,RUF,SIN,POUT,SUBPRP,
     #                  KXTEND)
            IF(POUT(3).LT.AREA)THEN
              J(NCHN+1)=I-1
              NCHN=NCHN+1
              DONE=1
              IF(G(I).GT.LOWER)THEN
                LOWER=G(I)
                Y0=LOWER
                A0=POUT(3)
              ENDIF
              IF(G(I-1).LT.HIGH)THEN
                HIGH=G(I-1)
              ENDIF
            ELSE
              HIGH=G(I)
              I=I+1
            ENDIF
          ELSE
            I=I+1
          ENDIF
          IF(DONE.NE.1.AND.I.LT.N) GO TO 10
        ENDIF
      ENDIF
      IF(I.LT.(K(NCHN)-1)) GO TO 15
        CALL COEFFI(J,K,NCHN,Y0,A0,S,G,K(NCHN),AREA,YS)
C       CALL PROPER(S,G,N,NSA,XSA,YS,NRUF,RUF,SIN,POUT,SUBPRP,KXTEND)
      ENDIF
      IF(OCHN.EQ.NCHN.AND.NCHN.GT.1) ERROR=2
C
C     solve for the appropiate area and stage
      IF(ERROR.EQ.0) THEN
        CALL PROPER(S,G,N,NSA,XSA,YS,NRUF,RUF,SIN,POUT,SUBPRP,KXTEND)
        AS=POUT(3)
        IF(AREA.GT.0)THEN
          PERR=ABS(AS-AREA)/AREA
        ELSE
          PERR=0.
        ENDIF
      ENDIF
      IF(PERR.GT.0.005.AND.NCHN.LE.1.AND.ERROR.EQ.0) GO TO 70
C
      RETURN
      END


      SUBROUTINE XSGEOM
     I                 (S,G,N,XSA,NSA,XI,
     O                  SUBPRP,LEL,REL,KXTEND)
C
C     + + + PURPOSE + + +
C     Subroutine xsgeom calculates areas, widths and wetted
C     perimeters for each specified subsection of a cross
C     section.  The assumptions used in writing this program
C     are, 1. S-G pairs must be ordered in a counter clockwise
C     sense.  2. Closure of the cross section is assumed only
C     when S(1)=S(N).  3. The cross section is augmented by
C     infinite vertical walls when water surface exceeds the
C     elevation of the banks.
C
C    + + + DUMMY ARGUMENTS + + +
      INTEGER N,NSA,KXTEND
      REAL S(N),G(N),XSA(NSA),SUBPRP(7,NSA),LEL,REL,XI
C
C     * * * ARGUMENT DEFINITIONS * * *
C          S - station (distance)
C          G - ground elevation measured from a datum
C          N - number of station, ground elevation pairs
C        XSA - subarea breakpoints
C        NSA - number of subareas
C         XI - stage
C      SUBPRP- subprp(1,isa)=WIDTH - channel width of subsection
C      SUBPRP- subprp(2,isa)= AREA - area of subsection
C      SUBPRP- subprp(3,isa)=PERIM - wetted perimeter of subsection
C      SUBPRP- subprp(4,isa)=ISTMOM- first moment of subsection about origin
C      SUBPRP- subprp(5,isa)- subarea stage
C      SUBPRP- subprp(6,isa)- conveyance subarea (reserved)
C      SUBPRP- subprp(7,isa)- subarea roughness
C     KXTEND - channel sides extended (bank overflow) on entry to subroutine
C              0 designates normal open channel(river, stream),
C              1 closed conduit channel (culvert, bridge cross-section)
C
C
C     * * * LOCAL VARIABLES * * *
      INTEGER ISA,ISA1,I,ISA0,FLG,IXMAX,IXMIN,ISUB(149),CLOSED
     #     ,IFLAT,KSUB
      REAL EWL,EWR,PERIMT,D1,S1,G1,Z,SUMH(149),XRIGHT,XLEFT,
     #     D0,S0,G0,DS,DG,B,A,P,H,EW,IMOVE,SWID(149)
C
C     + + + EXTERNALS + + +
      EXTERNAL IXMAX,IXMIN
C
C     + + + INTRINSICS + + +
      INTRINSIC ABS,AMAX1,SQRT
C
C
      DO 11 I=1,7
        DO 10 ISA=1,NSA
          SUBPRP(I,ISA)=0.
 10     CONTINUE
 11   CONTINUE
      DO 12 I=1,19
        ISUB(I)=0
        SUMH(I)=0.0
        SWID(I)=0.0
 12   CONTINUE
      PERIMT=0.
      EWL = +1E36
      EWR = -1E36
      CLOSED = 0
      IF(KXTEND.EQ.1) CLOSED=1
      KXTEND = 0
      ISA1=1
      LEL=XI
      REL=XI
C
C
      S1 = S(1)
      G1 = G(1)
      D1 = XI -G1
C     check for vertical extension of sides.
C     also checks for closure of figure.
      IF (D1.GT.0..AND.S1.LT.S(N)) THEN
        KXTEND = KXTEND+1
        SUBPRP(3,ISA1) = SUBPRP(3,ISA1)+D1
        EWL = S1
        LEL = XI
      ELSE IF (D1.GT.0.AND.(S1-S(N)).GE.0.) THEN
        LEL=G1
        EWL = S1
      ENDIF
      EW=EWL
C
C
C     begin computations of area, perimeter, etc.
      I=1
 30   CONTINUE
        I=I+1
        S0 = S1
        G0 = G1
        D0 = D1
        S1 = S(I)
        G1 = G(I)
        D1 = XI - G1
        ISA0 = ISA1
C       determine the subsection the coordinate pair S1, G1 is in.
        IF(S1.LE.XSA(1).OR.NSA.EQ.1) THEN
          ISA1=1
        ELSE IF(S1.GT.XSA(NSA-1)) THEN
          ISA1=NSA
        ELSE
          FLG=0
 40       CONTINUE
            IF(S1.GT.XSA(ISA1).AND.ISA1.NE.NSA) THEN
              ISA1=ISA1+1
            ELSE IF(S1.LE.XSA(ISA1-1)) THEN
              ISA1=ISA1-1
            ENDIF
            IF(S1.LE.XSA(ISA1).AND.S1.GT.XSA(ISA1-1)) FLG =1
          IF(S1.GT.XSA(ISA1).OR.S1.LE.XSA(ISA1-1)) GO TO 40
          IF(FLG.NE.1) GO TO 40
        ENDIF
        IF(D0.LE.0.AND.D1.LE.0..AND.I.LT.N) GO TO 30
C
C       compute a vertical coordinate for subsection breakpt if needed.
        IF(NSA.GT.1)THEN
        IF(S0.EQ.S1.AND.S1.EQ.XSA(ISA0))THEN
          IF(G1.LT.G0) THEN
            ISA0=ISA0+1
            ISA1=ISA0
          ENDIF
        ELSE IF(ISA0.LE.ISA1.AND.S0.EQ.XSA(ISA0)) THEN
          IF(S0.NE.S1) ISA0=ISA0+1
        ELSE IF(ISA0.GT.1)THEN
          IF(ISA0.GT.ISA1.AND.S0.EQ.XSA(ISA0-1))THEN
          ISA0=ISA0-1
          ENDIF
        ENDIF
        IF(ISA0.LT.ISA1) THEN
          G1=G0+(XSA(ISA0)-S0)*(G1-G0)/(S1-S0)
          S1=XSA(ISA0)
          D1=XI-G1
          ISA1=ISA0
          I=I-1
        ELSE IF(ISA0.GT.ISA1) THEN
          G1=G0+(XSA(ISA0-1)-S0)*(G1-G0)/(S1-S0)
          S1=XSA(ISA0-1)
          D1=XI-G1
          ISA1=ISA0
          I=I-1
        ENDIF
        ENDIF
C     calculate subsection geometric properties.
        DS = S1-S0
        DG = G1-G0
        IF (D0.GE.0..AND.D1.GE.0.) THEN
C         trapeziodal slice of area
          B = DS
          H=(D0+D1)*0.5
          A = DS*H
          P = SQRT(DS*DS+DG*DG)
          EW = S1
          Z=XI-AMAX1(G1,G0)
          IMOVE=0.5*DS*Z*Z + 0.5*DG*DS*((2.0*DG/3.0)+Z)
        ELSE IF((D0.GT.0.).OR.(D1.GT.0.)) THEN
C         triangular slice of area
          H = AMAX1(D0,D1)
          B = H*DS/ABS(DG)
          A = 0.5*B*H
          P = SQRT(B*B+H*H)
          EW = S0+(XI-G0)*DS/DG
          Z=XI-AMAX1(G1,G0)
          IMOVE=A*(2.0*Z/3)
          H = H/2.
        ELSE
          A = 0.
          H = 0.
          P = 0.
          B = 0.
          IMOVE=0.
        ENDIF
        IF(DG.GT.0..AND.EW.GE.EWR) THEN
          EWR=EW
c          REL=XI
          REL=G1
        ELSE IF(DG.LT.0..AND.EW.LT.EWL) THEN
          EWL=EW
          LEL=G1 + H*2.0
        ENDIF
        IF(B.GE.0.OR.CLOSED.EQ.1) SUBPRP(1,ISA1)=SUBPRP(1,ISA1)+B
        SUBPRP(2,ISA1) = SUBPRP(2,ISA1)+A
        IF((EW.GE.EWR).OR.(CLOSED.EQ.1))
     #    SUBPRP(3,ISA1) = SUBPRP(3,ISA1)+P
        PERIMT = PERIMT+P
        SUBPRP(4,ISA1)=SUBPRP(4,ISA1)+IMOVE
        IF(B.LT.0.0)THEN
C         weigh elevations by contributing width of trapezoid or triangle
          ISUB(ISA1)=ISUB(ISA1)+1
          SWID(ISA1)=SWID(ISA1)-B
          SUMH(ISA1)=SUMH(ISA1)-H*B
        ENDIF
      IF(I.LT.N) GO TO 30
C     extend sides of bank if needed
c      IF (S(N).GT.S(1).AND.G(N).LT.XI) THEN
      IF (S(N).GT.S(1)) THEN
        IF(D1.GT.0) THEN
          SUBPRP(3,ISA0)=SUBPRP(3,ISA0)+D1
          KXTEND = KXTEND+2
        ENDIF
        REL = XI
        IF(S(N).LT.EWR) EWR=S(N)
      ENDIF
      IF(EWL.GT.EWR) THEN
        IF(PERIMT.GT.0.) EWL=S(IXMAX(G,N))
        IF(PERIMT.LE.0.) EWL=S(IXMIN(G,N))
        EWR=EWL
      ENDIF
      DO 20 I=1,NSA
        IF(SUBPRP(1,I).EQ.0) THEN
          SUBPRP(5,I)=0.0
        ELSE
          SUBPRP(5,I)=XI-SUMH(I)/SUBPRP(1,I)
        ENDIF
20    CONTINUE
C
C     section added to compute topwidth (a real nonzero number)
C     for cases where the lowest part of the cross-section is flat
C     and zero area occurs for the stage computed.
      IFLAT=IXMIN(G,N)
      IF(XI-G(IFLAT).LE.0) THEN
        I=1
        XLEFT=S(IFLAT)
 55     CONTINUE
        IF(G(IFLAT).EQ.G(IFLAT+1)) THEN
 50       CONTINUE
          KSUB=0
          IF(S(IFLAT).GT.XSA(I)) THEN
            I=I+1
          ELSE
            KSUB=1
          ENDIF
          IF(KSUB.EQ.0.AND.I.LT.NSA) GO TO 50
          IF(KSUB.EQ.0.) I=NSA
          XRIGHT=S(IFLAT+1)
          IF(I.LT.NSA) THEN
            IF(XRIGHT.GT.XSA(I)) XRIGHT=XSA(I)
          ENDIF
          SUBPRP(1,I)=SUBPRP(1,I)+XRIGHT-XLEFT
          XLEFT=XRIGHT
          IFLAT=IFLAT+1
        ELSE
          IFLAT=N
        ENDIF
        IF(IFLAT.LT.N) GO TO 55
      ENDIF
C
      RETURN
      END
C
C
      INTEGER FUNCTION IXMAX(X,N)
C
C     + + + PURPOSE + + +
C    Finds the max value in array X and return location in array
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER N
      REAL X(N)
C
C     + + + ARGUMENT DEFINTIONS + + +
C     X     - array of values
C     N     - number of values in array
C
C     + + + LOCAL VARIABLES + + +
      INTEGER I
C
      IXMAX=1
      IF(N.GT.1) THEN
        DO 10 I=2,N
          IF(X(I).GT.X(IXMAX)) IXMAX=I
10      CONTINUE
      ENDIF
C
      RETURN
      END
C
C
      INTEGER FUNCTION IXMIN(X,N)
C
C     + + + PURPOSE + + +
C     Finds the min value in an array. Returns the position in array.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER N
      REAL X(N)
C
C     + + + LOCAL VARIABLE + + +
      INTEGER I
C
      IXMIN=1
      IF(N.GT.1) THEN
        DO 10 I=2,N
          IF(X(I).LT.X(IXMIN)) IXMIN=I
10      CONTINUE
      ENDIF
C
      RETURN
      END
C
C
      SUBROUTINE QUADEQ
     I               (A,B,C,
     O                X1,X2,DISCRM)
C
C     + + + PURPOSE + + +
C     QUAD solves for the roots of the quadratic equation, a(x**2)+bx+c=o
C
C     * * * DUMMY ARGUMENTS * * *
      REAL A,B,C,X1,X2,DISCRM
C
C     * * *  ARGUMENT DEFINITIONS * * *
C     A, B, C  - the coefficients from the quadratic equation
C     X1,X2    - the real roots of the quadratic equation
C     DISCRM   - used to determine if imaginary roots exist
C
C     * * * LOCAL VARIABLES * * *
C
C     + + + INTRINSIC + + +
      INTRINSIC SQRT
C
C
      DISCRM = B**2 - 4*A*C
      IF (DISCRM.GT.0) THEN
         X1 = (-B + SQRT(DISCRM))/(2*A)
         X2 = (-B - SQRT(DISCRM))/(2*A)
      ELSE IF (DISCRM.EQ.0) THEN
         X1 = -B/(2*A)
      ENDIF
C
      RETURN
      END
      SUBROUTINE COEFFI
     I                 (I,K,NCHN,Y0,A0,X,Y,NPTS,AREA,
     O                  STAGE)
C
C     + + + PURPOSE + + +
C     This program solves for stage the quadratic equation
C     ay**2 + by + c = 0 that results from solving for stage given
C     the cross section area of a channel.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER NCHN,I(NCHN),K(NCHN),NPTS
      REAL X(NPTS),Y(NPTS),AREA,STAGE,Y0,A0
C
C     + + + ARGUMENT DEFINTIONS + + +
C     I      - INDEX NO. OF BEGINNING PAIR
C     K      - INDEX NO. OF ENDING PAIR
C     X      - X COORDINATE
C     Y      - Y COORDINATE
C     NPTS   - NO. OF COORDINATE PAIRS
C     AREA   - GIVEN CROSS SECTION AREA OF CHANNEL TO SOLVE YS AT
C     NCHN   - NUMBER OF CHANNELS
C     Y0     - STAGE AT WHICH A0 IS VALID
C     A0     - AREA AT Y0
C     STAGE  - the stage obtained from solving the quadratic
C
C     + + + LOCAL VARIABLES + + +
      INTEGER IK,II,KK,FLG
      REAL SLOPEL,SLOPER,SUMXR,SUMXL,SLPLEF,SLPRGT, A, B, C
      REAL DISCRM,X1,X2,SMYSR,SMYSL
C
C     + + + EXTERNALS + + +
      EXTERNAL QUADEQ
C
C     + + + INTRINSIC + + +
      INTRINSIC SQRT
C
      SLOPER=0.
      SLOPEL=0.
      SUMXR=0.
      SUMXL=0.
      SMYSR=0.
      SMYSL=0.
      DO 10 IK=1,NCHN
        II=I(IK)
        KK=K(IK)
        SUMXR=SUMXR+X(KK)
        SUMXL=SUMXL+X(II)
        SLPLEF = (X(II+1)-X(II))/(Y(II+1)-Y(II))
        SLPRGT = (X(KK)-X(KK-1))/(Y(KK)-Y(KK-1))
        SLOPEL = SLOPEL + SLPLEF
        SLOPER = SLOPER + SLPRGT
        SMYSR = SMYSR+SLPRGT*(Y(KK)-Y0)
        SMYSL = SMYSL+SLPLEF*(Y(II)-Y0)
10    CONTINUE
C
      A = (SLOPER - SLOPEL)*(0.5)
      B = SUMXR-SUMXL-SMYSR+SMYSL
      C = A0-AREA
C
        IF(A.NE.0.0) THEN
C       AREA IS A RECTANGLE
          CALL QUADEQ(A,B,C,X1,X2,DISCRM)
          IF(DISCRM.LT.0) THEN
            FLG=1
          ELSE IF(DISCRM.GT.0) THEN
            IF(X1.GT.X2) THEN
              STAGE=X1+Y0
            ELSE IF(X2.GT.X1) THEN
              STAGE=X2+Y0
            ENDIF
          ELSE IF(DISCRM.EQ.0) THEN
           IF(X1.GT.0) STAGE =X1
          ENDIF
        ELSE
          STAGE=Y0-C/B
        ENDIF
C
      RETURN
      END

       SUBROUTINE RAMP
     I               (DEP,N,VAL,ISA,
     M                IND,
     O                EST)
C
C     + + + PURPOSE + + +
C     fits linearly between points and uses values specified at
C     endpoints.
C
C     + + + DUMMY ARGUMENTS + + +
      INTEGER N,ISA,IND
      REAL DEP,VAL(3,N),EST
C
C     + + + INTRINSIC + + +
      INTRINSIC INT
C
C     + + + LOCAL VARIABLES + + +
      INTEGER I,K,FLG,IEND
      REAL W
C
C
      K=0
      FLG=1
10    CONTINUE
        K=K+1
      IF(ISA.GT.INT(VAL(1,K)).AND.K.LT.N) GO TO 10
      IND=K
      IF(K.LT.N)THEN
20      CONTINUE
        FLG=0
        K=K+1
        IF(ISA.GE.INT(VAL(1,K))) FLG=1
        IF(FLG.EQ.1.AND.K.LT.N) GO TO 20
      ENDIF
      IF(FLG.NE.1) THEN
        IEND=K-1
      ELSE
        IEND=N
      ENDIF
C
      IF(DEP.GT.VAL(3,IND).AND.DEP.LE.VAL(3,IEND)) THEN
        I=IND
30     CONTINUE
        I=I+1
       IF(DEP.GT.VAL(3,I)) GO TO 30
        W=(DEP-VAL(3,I-1))/(VAL(3,I)-VAL(3,I-1))
        EST=VAL(2,I-1)*(1.-W)+VAL(2,I)*W
      ELSE IF(DEP.LE.VAL(3,IND)) THEN
        EST=VAL(2,IND)
      ELSE IF(DEP.GT.VAL(3,IEND)) THEN
        EST=VAL(2,IEND)
      ENDIF
C
      RETURN
      END
C
C

