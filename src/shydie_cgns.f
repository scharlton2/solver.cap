      SUBROUTINE HYDIE_CGNS(SI)

      IMPLICIT NONE

C-----Purpose:
C     load approach information from CGNS file.
      INTEGER NXSEC,NCORD,NSUBS,NSBS
      PARAMETER (NXSEC=200, NCORD=150, NSUBS=149, NSBS=300)

C-----Arguments:
      INTEGER SI

C     + + + LOCAL VARIABLES + + +
      INTEGER NKTS,I,NCOMP,K,IREC,II,N,ERR,EDF,NRUF,NOUT,MATCH,
     #       NHDR,NCARDS,XSREC(NXSEC),KXND,ERFG,EREC,NSIN
     #       ,FNDXSC,J,ISEC,OPOUT(17),NSA,BRNF,NSECT
     #       ,IHDR,IOVER,ERRNO,IOLD,NSTOP,KTS,NOHP,KK,SL1
C
      REAL S(NCORD),G(NCORD),XSA(NSUBS),POUT(17,1),SIN(3,NSBS),
     #  RATIO,RUF(3,NSBS),MAXDEP,X(NXSEC),SRD,
     #  MINDEP,SUBPRP(7,NSUBS),DATUM,
     #  DMAX
      CHARACTER*16 SECID

C     + + + EXTERNALS + + +
      INTEGER MATSTR,IXMAX,IXMIN
      EXTERNAL WSPDA,HYDDA,WSPSEQ,KNOTS,WSPCDS,PROPER,STAGE,HYDOUT,
     #         FNDXSC,OUTTAB,IXMIN,BRNOUT,MATSTR,IXMAX
C

      CALL WSPCDS_CGNS(S,G,N,NSA,XSA,NRUF,RUF,SRD,DMAX,
     #                 RATIO,SI,EDF)
      J=IXMIN(G,N)
      MINDEP=G(J)
      MAXDEP=G(J)+DMAX
      NKTS=25
      DO I=1,NSBS
        SIN(1,I)=0.0
        SIN(2,I)=1.0
        SIN(3,I)=0.0
      END DO

      CALL KNOTS(NKTS,MINDEP,MAXDEP,RATIO,X,ERR)
      SECID='AP01            '
      DO II=1,NKTS
        CALL PROPER(S,G,N,NSA,XSA,X(II),NRUF,RUF,SIN,POUT,SUBPRP
     #       ,KXND)
        CALL CULOUT(SRD,SECID,II,NKTS,POUT)
      END DO
      RETURN
      END
C

      SUBROUTINE WSPCDS_CGNS(S,G,N,NSA,XSA,NRUF,RUF,SRD,
     #  DMAX,RATIO,SI,EDFG)

      IMPLICIT NONE

C-----Purpose:
C     Reads a single x-section from cgns file

C-----Parameters:
      INTEGER NCORD,NSUBS,NSBS
      PARAMETER (NCORD=150, NSUBS=149, NSBS=300)

C-----Arguments:
      INTEGER ERRIO,NRUF,N,NSA,EDFG,SI
      REAL S(NCORD),G(NCORD),XSA(NSUBS),RUF(3,NSBS),SRD
      REAL DMAX,RATIO,N_SIMPLE
      real:: m_to_ft

C     + + + ARGUMENT DEFINITIONS + + +
C     S       - station point
C     G       - ground elevation
C     N       - no. of station points and groung elevation pairs
C     NSA     - no. of subsection areas
C     XSA     - array of subsection righthand breakpoints.
C     NRUF    - no. of roughness values
C     RUF     - array of roughness data
C     DMAX    - maximum depth
C     RATIO   - the common ratio used to compute depths
C     SRD     - cross section reference distance in feet
C     EDFG    - error flag
C
C     + + + LOCAL VARIABLES + + +
      INTEGER FLG,I,J,NCHAR,MAX,NCNT,TMPFLG,MATSTR,IXMIN,
     #        IJ,ERRNO
     #        ,RREC,IOVER,N_TYPE,IER
      REAL  SKEW,SKEW_ANGLE
      CHARACTER  CBUF*77,CHRDUM*77
      CHARACTER*3 CODE,COM,SINJNK
      CHARACTER*16 BLKOUT,SECID
      REAL N_TMP(NCORD),BOTD(NCORD),TOPD(NCORD),BOTN(NCORD),TOPN(NCORD)
C
C     + + + EXTERNALS + + +
      EXTERNAL MATSTR,IXMIN

      CHARACTER*10 ITOSTR,FTOSTR
      EXTERNAL ITOSTR,FTOSTR
C
      DATA IOVER / 1000 /
C
      ERRIO=0
      NCHAR=70
      SKEW=1.0
      SECID='AP01            '

 101  FORMAT(A)

      CALL CG_IRIC_READ_REALSINGLE_F('appr_skew'    , SKEW_ANGLE, IER)
      CALL CG_IRIC_READ_REALSINGLE_F('max_depth'    , DMAX,       IER)
      CALL CG_IRIC_READ_REALSINGLE_F('spacing_ratio', RATIO,      IER)
      SKEW=COS(3.14159 * SKEW_ANGLE / 180.0)
      
      CALL CG_IRIC_READ_REALSINGLE_F('srd_approach' , SRD,        IER)

      CALL CG_IRIC_READ_INTEGER_F('n_type'       , N_TYPE,     IER)

      WRITE(ERRIO,101) 'XS   AP01 ' // TRIM(FTOSTR(SRD)) // ',' //
     #  TRIM(FTOSTR(SKEW_ANGLE))
      WRITE(ERRIO,101) '*PD       0,' // TRIM(FTOSTR(DMAX)) // ',' //
     #  TRIM(FTOSTR(RATIO))

      IF (N_TYPE == 0) THEN
C       Simple
        CALL CG_IRIC_READ_FUNCTIONALSIZE_F('appr_xs', N, IER)
        CALL CG_IRIC_READ_FUNCTIONAL_REALSINGLE_F('appr_xs',
     #    S, G, IER)
        CALL CG_IRIC_READ_REALSINGLE_F('n_simple'   , N_SIMPLE,   IER)

        NRUF=1
        RUF(1,1)=1
        RUF(2,1)=N_SIMPLE
        RUF(3,1)=0
        NSA=1
        XSA(1)=0
      ELSE IF (N_TYPE == 1) THEN
C       Horizontal distribution
        CALL CG_IRIC_READ_FUNCTIONALSIZE_F('n_h', N, IER)
        CALL CG_IRIC_READ_FUNCTIONALWITHNAME_REALSINGLE_F('n_h',
     #    'X', S, IER)
        CALL CG_IRIC_READ_FUNCTIONALWITHNAME_REALSINGLE_F('n_h',
     #    'Y', G, IER)
        CALL CG_IRIC_READ_FUNCTIONALWITHNAME_REALSINGLE_F('n_h',
     #    'NVAL', N_TMP, IER)

        NRUF=1
        RUF(1,1)=1
        RUF(2,1)=N_TMP(2)
        RUF(3,1)=0
        NSA=1
        XSA(1)=0

        DO I=3,N
          IF (N_TMP(I).NE.RUF(2,NRUF)) THEN
            NRUF=NRUF+1
            RUF(1,NRUF)=NRUF
            RUF(2,NRUF)=N_TMP(I)
            RUF(3,NRUF)=0
            NSA=NSA+1
            XSA(NSA-1)=(S(I-1))
          ENDIF
        END DO
      ELSE IF (N_TYPE == 2) THEN
C       Vertical distribution
        CALL CG_IRIC_READ_FUNCTIONALSIZE_F('n_h_v', N, IER)
        CALL CG_IRIC_READ_FUNCTIONALWITHNAME_REALSINGLE_F('n_h_v', 'X',
     #    S, IER)
        CALL CG_IRIC_READ_FUNCTIONALWITHNAME_REALSINGLE_F('n_h_v', 'Y',
     #    G, IER)
        CALL CG_IRIC_READ_FUNCTIONALWITHNAME_REALSINGLE_F('n_h_v', 
     #    'BOTD', BOTD, IER)
        CALL CG_IRIC_READ_FUNCTIONALWITHNAME_REALSINGLE_F('n_h_v',
     #    'TOPD', TOPD, IER)
        CALL CG_IRIC_READ_FUNCTIONALWITHNAME_REALSINGLE_F('n_h_v',
     #    'BOTN', BOTN, IER)
        CALL CG_IRIC_READ_FUNCTIONALWITHNAME_REALSINGLE_F('n_h_v',
     #    'TOPN', TOPN, IER)
        NRUF=2
        RUF(1,1)=1
        RUF(2,1)=BOTN(2)
        RUF(3,1)=BOTD(2)
        RUF(1,2)=1
        RUF(2,2)=TOPN(2)
        RUF(3,2)=TOPD(2)
        NSA=1
        XSA(1)=0
        
        DO I=3,N
          IF ((BOTN(I).NE.RUF(2,NRUF-1)).OR.
     #        (BOTD(I).NE.RUF(3,NRUF-1)).OR.
     #        (TOPN(I).NE.RUF(2,NRUF)).OR.
     #        (TOPD(I).NE.RUF(3,NRUF))) THEN
            NRUF=NRUF+2
            RUF(1,NRUF-1)=NRUF/2
            RUF(2,NRUF-1)=BOTN(I)
            RUF(3,NRUF-1)=BOTD(I)
            RUF(1,NRUF  )=NRUF/2
            RUF(2,NRUF  )=TOPN(I)
            RUF(3,NRUF  )=TOPD(I)
            NSA=NSA+1
            XSA(NSA-1)=(S(I-1))
          ENDIF
        END DO
      ENDIF

C     Output log

      CBUF = ''
      DO I=0, N-1
        IF (MOD(I, 6) == 0 .AND. I /= 0) THEN
          WRITE(ERRIO,101) 'GR        ' // TRIM(CBUF)
          CBUF = ''
        ENDIF
        
        CBUF = TRIM(ADJUSTL(CBUF)) // ' ' // TRIM(FTOSTR(S(I+1))) //
     #    ',' // TRIM(FTOSTR(G(I+1)))
      END DO
      IF (TRIM(CBUF) /= '') THEN
        WRITE(ERRIO,101) 'GR        ' // TRIM(ADJUSTL(CBUF))
      ENDIF

      IF (N_TYPE == 0) THEN
        WRITE(ERRIO,101) 'N         ' // TRIM(FTOSTR(N_SIMPLE))
      ELSE IF (N_TYPE == 1) THEN
        CBUF = ''
        DO I=0, NRUF-1
          IF (MOD(I, 6) == 0 .AND. I /= 0) THEN
            WRITE(ERRIO,101) 'N         ' // TRIM(CBUF)
            CBUF = ''
          ENDIF
          CBUF = TRIM(ADJUSTL(CBUF)) // ' ' // TRIM(FTOSTR(RUF(2, I+1)))
        END DO
        IF (TRIM(CBUF) /= '') THEN
          WRITE(ERRIO,101) 'N         ' // TRIM(ADJUSTL(CBUF))
        ENDIF
        
        CBUF = ''
        DO I=0, NSA-2
          IF (MOD(I, 6) == 0 .AND. I /= 0) THEN
            WRITE(ERRIO,101) 'SA        ' // TRIM(CBUF)
          ENDIF
          CBUF = TRIM(ADJUSTL(CBUF)) // ' ' // TRIM(FTOSTR(XSA(I+1)))
        END DO
        IF (TRIM(CBUF) /= '') THEN
          WRITE(ERRIO,101) 'SA        ' // TRIM(CBUF)
        ENDIF
      ELSE IF (N_TYPE == 2) THEN
        CBUF = ''
        DO I=0, (NRUF/2)-1
          IF (MOD(I, 6) == 0 .AND. I /= 0) THEN
            WRITE(ERRIO,101) 'N         ' // TRIM(CBUF)
            CBUF = ''
          ENDIF
          CBUF = TRIM(ADJUSTL(CBUF)) // ' ' //
     #      TRIM(FTOSTR(RUF(2, I*2+1))) // ',' //
     #      TRIM(FTOSTR(RUF(2, I*2+2)))
        END DO
        IF (TRIM(CBUF) /= '') THEN
          WRITE(ERRIO,101) 'N         ' // TRIM(ADJUSTL(CBUF))
        ENDIF
             
        CBUF = ''
        DO I=0, (NRUF/2)-1
          IF (MOD(I, 6) == 0 .AND. I /= 0) THEN
            WRITE(ERRIO,101) 'ND        ' // TRIM(CBUF)
            CBUF = ''
          ENDIF
          CBUF = TRIM(ADJUSTL(CBUF)) // ' ' //
     #      TRIM(FTOSTR(RUF(3, I*2+1))) // ',' // 
     #      TRIM(FTOSTR(RUF(3, I*2+2)))
        END DO
        IF (TRIM(CBUF) /= '') THEN
          WRITE(ERRIO,101) 'ND        ' // TRIM(ADJUSTL(CBUF))
        ENDIF

        CBUF = ''
        DO I=0, NSA-2
          IF (MOD(I, 6) == 0 .AND. I /= 0) THEN
            WRITE(ERRIO,101) 'SA        ' // TRIM(CBUF)
          ENDIF
          CBUF = TRIM(ADJUSTL(CBUF)) // ' ' // TRIM(FTOSTR(XSA(I+1)))
        END DO
        IF (TRIM(CBUF) /= '') THEN
          WRITE(ERRIO,101) 'SA        ' // TRIM(ADJUSTL(CBUF))
        ENDIF
      ENDIF

      DO I=1,N
        S(I)=S(I) * SKEW
      END DO
      DO I=1,NSA
        XSA(I)=XSA(I) * SKEW
      END DO

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
      IF (SI.EQ.1.OR.SI.EQ.3) THEN
C       convert meters to feet
        call m_to_ft_array(N,S)
        call m_to_ft_array(N,G)
        call m_to_ft_array(NSA,XSA)
        SRD = m_to_ft(SRD)
        DMAX = m_to_ft(DMAX)
        DO I=1,NRUF
          RUF(3,I) = m_to_ft(RUF(3,I))
        END DO
      ENDIF
C
      RETURN
      END
