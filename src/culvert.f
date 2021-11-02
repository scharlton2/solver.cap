C=====CLTST Bof==============================================================C
C-----Purpose:
C       testing routine for culvert module & fcncn2 common
C     Programmed by: JM Fulford
C     Revised Dec 2011 to allow all inputs on command line and to allow interactive input of file names and section ids
C     For command line inputs the order is:  1-Output file name, 2-Culvert data file, 3-culvert id, 4-Approach data file, 
C          5-approach id, 6-output header string
C     Modified by gfkoltun for 2021 version      
C
      PROGRAM CLTST

      use iric
      use iricio

      IMPLICIT NONE

      include 'CULVRT.INC'

C-----Local variables:
      INTEGER OTNIT,GSN,IREPORT,PNO,ERR,IER,CGNSOUT
      CHARACTER*3 RERUN
      CHARACTER*16 XSCID,CLID,BLKOUT
      CHARACTER*132 DUMPY
      CHARACTER*80 HTITLE
      REAL BASEL,BASEX
C
C-----Externals:
      LOGICAL ICVGEO,IXSGEO,ICVPRP,IXSPRP,ICVPRP_CGNS,IXSPRP_CGNS
      EXTERNAL ICVGEO,IXSGEO,ICVPRP,IXSPRP,ICVPRP_CGNS,IXSPRP_CGNS
C-----trap ieee float point division by zero - sun version
C      INTEGER myhandler
C      EXTERNAL myhandler
C      ieeer = ieee_handler('set','division',myhandler)
C-----      
C     changed Feb 8, 2011 to allow batch running of the program.  Only detailed table is producted and a default title is used
C
C     open culvert data file
      BASEL = 0.0 
      CALL GETARG (1,DUMPY)
      CGNSOUT=0
      IF (DUMPY == 'Case1.cgn') THEN
        CGNSOUT=1
C       read setting from CGNS file
        ERR=1
C       output to stdout
        OTNIT=6
        IREPORT=1
C       Versions before 2011 started at page 0. I modified to start at page 1 (gfk)
        PNO=1
        CALL VERSON(OTNIT,PNO)
        ERR=2

        CALL CG_IRIC_OPEN(DUMPY, IRIC_MODE_MODIFY, FID, IER)

        IF(ICVPRP_CGNS(DUMPY,BASEL)) ERR=0
        CLID ='EX01            '
        XSCID='AP01            '
         
        IF(IXSPRP_CGNS(XSCID,BASEL,SI)) ERR=0
        HTITLE = 'iRIC CAP PROJECT'

C       write culvert table to output
        CALL WCULTB(OTNIT,PNO,HTITLE)

C       write approach properties table to output
        CALL APPDMP(OTNIT,PNO,HTITLE)

C       computing culvert flow
        CALL CULRAT(XSCID,CLID)

C       write flow solutions to output
        CGNSOUT=1
        CALL HARDCPY(OTNIT,PNO,HTITLE,CGNSOUT)

        CALL CG_IRIC_CLOSE(FID, IER)

      ELSE IF (LEN_TRIM(DUMPY).GT.0) THEN
C       read file names, section ids and header text in this section
        ERR=1
        OTNIT=9
        IREPORT =1
        OPEN(UNIT=OTNIT,FILE=DUMPY,STATUS='NEW',ERR=101)
        ERR=0
 101    CONTINUE
        IF (ICVGEO()) THEN
          CALL GETARG(3,CLID)
          PNO=1
          CALL VERSON(OTNIT,PNO)
          ERR=2
          IF (ICVPRP(CLID,BASEL)) ERR=0   
          IF (IXSGEO()) THEN
            CALL GETARG(5,XSCID)
            ERR=2
            IF (IXSPRP(XSCID,BASEL)) ERR=0
          ENDIF
        ENDIF
        PNO=1
        CALL GETARG(6,HTITLE)

C       write culvert table to output
        CALL WCULTB(OTNIT,PNO,HTITLE)

C       write approach properties table to output
        CALL APPDMP(OTNIT,PNO,HTITLE)

C       Computing culvert flow
        CALL CULRAT(XSCID,CLID)

C       write flow solutions to output
        CALL HARDCPY(OTNIT,PNO,HTITLE,CGNSOUT)
        CLOSE(UNIT=OTNIT)

      ELSE
C       query the user with DOS command line text queries as in version 97-08b
        PNO=-1
        OTNIT=0
        CALL VERSON(OTNIT,PNO)
50      CONTINUE
        BLKOUT='                '
        DUMPY=BLKOUT//BLKOUT
        XSCID=BLKOUT
        CLID=BLKOUT
        RERUN=BLKOUT(1:3)
        OTNIT=9
        WRITE(*,*) 'SPECIFY TYPE OF PROGRAM OUTPUT, 1-detailed report;'
     #   ,' 2-report & table'
        READ(*,'(I1)')IREPORT
        WRITE(*,*)' '
        WRITE(*,*)'Enter output file name:'
 202    CONTINUE
        READ(*,'(A)')DUMPY
        ERR=1
        OPEN(UNIT=OTNIT,FILE=DUMPY,STATUS='NEW',ERR=201)
        ERR=0
 201    CONTINUE
        IF(ERR.EQ.1)THEN
          WRITE(*,*)'File exists or can''t be opened - enter new name:'
          GO TO 202
        ENDIF

        WRITE(*,*)'Enter header title for output file:'
        READ(*,'(A80)')HTITLE

        IF(IREPORT.EQ.2)THEN
          WRITE(*,*)'Enter table ID number (8 digits):'
          READ(*,*) GSN
        ENDIF

        IF (ICVGEO()) THEN
C       open culvert data file
 211      CONTINUE
          WRITE(*,*) 'Enter culvert section id:'
          READ(*,'(A16)') CLID
          PNO=1
          CALL VERSON (OTNIT,PNO)
          IF(.NOT.ICVPRP(CLID,BASEL)) THEN
            ERR=2
            WRITE(*,*) '..RETRY(Y/N)?'
            READ(*,'(A)')RERUN
            WRITE(*,*)RERUN
            IF (RERUN(1:1).EQ.'N'.OR.RERUN(1:1).EQ.'n') ERR=1
          ELSE
            ERR=0
            WRITE(*,*)'CULVERT SECTION '//TRIM(CLID)//' FOUND'
          ENDIF
          IF (ERR.EQ.2) GOTO 211
          IF (ERR.NE.0) GOTO 999
          IF (IXSGEO()) THEN
 212        CONTINUE
            WRITE(*,*) 'Enter approach section id:'
            READ(*,'(A16)')XSCID
            BASEX=BASEL
            IF (.NOT.IXSPRP(XSCID,BASEX)) THEN
              ERR=2
              IF (BASEX.GT.-30) THEN
                WRITE(*,*)'...RETRY(Y/N)?'
                READ(*,'(A)')RERUN
              ELSE
                RERUN='N'
              ENDIF
              IF (RERUN(1:1).EQ.'n'.OR.RERUN(1:1).EQ.'N') ERR=1
            ELSE
              ERR=0
              WRITE(*,*)'Approach section '//TRIM(XSCID)//' found'
            ENDIF
            IF(ERR.EQ.2) GOTO 212
            IF(ERR.EQ.1) GOTO 200
            PNO=1

C           write culvert table to output
            CALL WCULTB(OTNIT,PNO,HTITLE)

C           write approach properties table to output
            CALL APPDMP(OTNIT,PNO,HTITLE)

C           Computing culvert flow
            CALL CULRAT(XSCID,CLID)

C           write flow solutions to output
            CALL HARDCPY(OTNIT,PNO,HTITLE,CGNSOUT)

            CLOSE(UNIT=OTNIT)
            IF (IREPORT.EQ.2) THEN
              OPEN(UNIT=OTNIT,FILE='TABLES30.DAT',STATUS='NEW')
              WRITE(OTNIT,'(A12)')'TABLES30.DAT'
              CALL TAB3PAR(GSN,OTNIT)
              CLOSE(UNIT=OTNIT)
            ENDIF
 200        CONTINUE
          ELSE
            WRITE(*,*)'Approach file not found -- CAP not executed'
          ENDIF
        ELSE
          WRITE(*,*)'Culvert file not found -- CAP not executed'
        ENDIF
999     CONTINUE
        WRITE(*,*)'EXIT CAP? (enter YES or NO)'
        READ(*,'(A)')RERUN
        IF(RERUN(1:1).EQ.'n'.OR.RERUN(1:1).EQ.'N') GOTO 50
      ENDIF
C
      STOP
      END
C
C=====CLTST Eof=============================================================
C=====myhandler bof=========================================================
C      handler for trapping float point exceptions on the sun
C      INTEGER FUNCTION myhandler( sig, code, context)
C      INTEGER sig, code, context(5)
C      CALL abort()
C      END
C=====