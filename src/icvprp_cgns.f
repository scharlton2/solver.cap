C=====ICVPRP_CGNS bof========================================================

C-----Purpose:
C       convert int value to simple string
C     Programmed by: Keisuke Inoue

      CHARACTER*10 FUNCTION ITOSTR(intval)
        INTEGER, INTENT(IN):: intval
        
        CHARACTER*10 buf
 300    FORMAT(I10)
        WRITE(buf,300) intval
        ITOSTR = TRIM(ADJUSTL(buf))
      END FUNCTION ITOSTR
      
C-----Purpose:
C       convert float value to simple string
C     Programmed by: Keisuke Inoue

      CHARACTER*10 FUNCTION FTOSTR(floatval)
        REAL, INTENT(IN):: floatval
        INTEGER:: strlen
        
        CHARACTER*10 buf
 301    FORMAT(F10.4)
        WRITE(buf,301) floatval
        buf = ADJUSTL(buf)
        
 302    CONTINUE
        strlen = len_trim(buf)
        IF (buf(strlen:strlen) == '0') THEN
          buf = buf(1:strlen-1)
          GOTO 302
        ENDIF
        strlen = len_trim(buf)
        IF (buf(strlen:strlen) == '.') THEN
          buf = buf(1:strlen-1)
        ENDIF
        FTOSTR = TRIM(buf)
      END FUNCTION FTOSTR
      
C-----Purpose:
C       initialize culvert properties table
C     Programmed by: Keisuke Inoue

      logical function ICVPRP_CGNS(cgns_filename,basel)
      implicit none
    
C-----Arguments:
      CHARACTER(LEN=*), INTENT(IN) :: CGNS_FILENAME
      REAL, INTENT(OUT) :: BASEL
C      INTEGER :: IER

      include 'CULVRT.INC'

C-----Local variables:
      INTEGER :: NCS,ERR,I,WEB,ERR1,TCR
      REAL    :: B,D,N,SCUL(MXCV),GCUL(MXCV),TW


C-----local definitions:
C     culnit - unit number to open wspro file containing culvert data on
C     tc     - culvert code type; 1-circular, 2-box, 3-pipe arch, 4-nonstand.
C     ncs    - no. of coordinates describing nonstandard culvert section
C     b      - max. span of culvert, or width for box culvert
C     d      - max. rise of culvert, height for box culvert or diameter for
C              circular culvert
C     web    - no. of webs in culvert
C     n      - culvert roughness, manning's n
C     scul   - x coordinate for non-standard culvert section
C     gcul   - y coordinate for non-standard culvert section

C-----Externals:
      INTEGER IXMIN,IXMAX
      EXTERNAL IXMIN,IXMAX

      ICVPRP_CGNS = .FALSE.

      NCS = 0
C     read data from cgns file
      CALL RCVGEO_CGNS(BASEL,D,B,WEB,N,SCUL,GCUL,NCS,ERR)
      CWIDTH=B
      IF (ERR == 1) THEN
        ERR1=-8
        CALL EROCUL(ERR1,I)
      ENDIF
      IF(ERR.NE.0) GO TO 999

C     Conversion to feet for height and width of culvert
      TCR=INT(TC/100)
      NRUFF=N

      CALL CALCCULVPROP(TCR,D,B,WEB,NCS,SCUL,GCUL)

      TC=INT(TC/10)
C
      ICVPRP_CGNS=.TRUE.

999   CONTINUE
      RETURN
      END FUNCTION
C=====ICVPRP_CGNS eof=================================================

C=====RCVGEO_CGNS bof=================================================
C
C-----Purpose:
C       read culvert data from CGNS file
C     Programmed by: Keisuke Inoue
C     Date:
C     Modified by: gfkoltun for 2021 version
C     Last modified:
C
      SUBROUTINE RCVGEO_CGNS(DATUM,DR,B,WEB,N,SCUL,GCUL,NCS,ERR)

      IMPLICIT NONE

C-----Module data:
      INCLUDE 'CULVRT.INC'
      INCLUDE 'CULFLW.INC'

C-----Arguments:
      INTEGER WEB,NCS,TCR,ERR
      REAL DR,B,N,SCUL(MXCV),GCUL(MXCV),DATUM

C-----Argument definitions:
C     CULNIT - file unit number to read wspro data from
C     CID  - culvert id to fetch
C     TC     - culvert code type; 1-circular, 2-box, 3-pipe arch, 4-nonstand.
C     B      - max. span of culvert, or width for box culvert
C     D      - max. rise of culvert, height for box culvert or diameter for
C              circular culvert
C     WEB    - no. of webs in culvert
C     NCS    - no. of coordinates describing nonstandard culvert section
C     N      - culvert roughness, Manning's n
C     SCUL   - x coordinate for non-standard culvert section
C     GCUL   - y coordinate for non-standard culvert section
C
C
C-----Externals:
      REAL WINGWALL, QC46,KBEV,KRND,KPRJCT
      EXTERNAL WINGWALL, QC46,KBEV,KRND,KPRJCT

      CHARACTER*10 ITOSTR,FTOSTR
      INTEGER IXMIN,IXMAX
      EXTERNAL ITOSTR,FTOSTR
      EXTERNAL IXMIN,IXMAX
C
C-----Local Variables:
      INTEGER FLG,I,CQFG,CSFG,CXFG,ERR1,ERR2,NC5
      INTEGER READY,ERRIO,IER,ITOP,IBOT
      INTEGER SHAPE_TYPE,MATERIAL,EDGE_TYPE,PROJ_TYPE,ALI_TYPE
      INTEGER WW_TYPE,C1_ENABLE,C5_ENABLE,DISC_METHOD
      REAL DUMMYIDS(MAXTW)
      REAL BEVD,RNDD,LPRJCT,DSINV,USINV
      REAL LPROJ1,LPROJ2
      REAL THETA0,THETA1,THETA2,TMP_KR,TMP_KW
      REAL LCUL1,LCUL2,LCUL3,LMAX,LMIN,HWD
      REAL KWING1,KWING2
      CHARACTER*80 CBUF
C      CHARACTER*3 CODE
C
C-----Formats
C 100  FORMAT(A3,A77)
C
      ERRIO=0
      DO I=1,3
        ARAD(I)=0.0
      END DO

      CXFG=0
      CQFG=0
      CSFG=0
      FLG=0
      ERR=1
      BASEL=0.0
      NHP=-1
      RND=0.0
      KWING=1.0
      RNDD=0.0
      BEVD=0.0
      KWR=1.0
      KPROJ=1.0
      KWING=1.0
      TFLW=65
      HFLW=100000.
      READY=0
      SI=0
      INLET=-1
      C46=0.
      LPRJCT=0.0
      NH5 = -1

C     Always use 'EX01' for culvert id
      VRTID='EX01            '
      CULID='EX01            '

C     -----------------------------------------------
C     Read data that corresponds to SI from CGNS file
C     -----------------------------------------------

      CALL cg_iric_read_integer_f('units', SI, IER)

 101  FORMAT(A)
      WRITE(ERRIO,101) 'SI        ' // TRIM(ITOSTR(SI))

C     -----------------------------------------------
C     Read data that corresponds to CG from CGNS file
C     -----------------------------------------------

      CALL cg_iric_read_integer_f('shape_type', SHAPE_TYPE, IER)
      CALL cg_iric_read_integer_f('material', MATERIAL, IER)

      IF (SHAPE_TYPE == 0) THEN
C       Select type
C       @TODO SHOW ERROR
      ELSE IF (SHAPE_TYPE == 1) THEN
C       Box
        TCR = 1
        CALL CG_IRIC_READ_REALSINGLE_F('rise'    , D       , IER)
        CALL CG_IRIC_READ_REALSINGLE_F('span'    , B       , IER)
      ELSE IF (SHAPE_TYPE == 2) THEN
C       circular
        TCR = 2
        CALL CG_IRIC_READ_REALSINGLE_F('diameter', D       , IER)
        B = D
      ELSE IF (SHAPE_TYPE == 3) THEN
C       elliptical
        TCR = 2
        CALL CG_IRIC_READ_REALSINGLE_F('rise'    , D       , IER)
        CALL CG_IRIC_READ_REALSINGLE_F('span'    , B       , IER)
      ELSE IF (SHAPE_TYPE == 4) THEN
C       pipe arch
        TCR = 3
        CALL CG_IRIC_READ_REALSINGLE_F('rise'    , D       , IER)
        CALL CG_IRIC_READ_REALSINGLE_F('span'    , B       , IER)
        CALL CG_IRIC_READ_REALSINGLE_F('rbottom' , ARAD(1) , IER)
        CALL CG_IRIC_READ_REALSINGLE_F('rtop'    , ARAD(2) , IER)
        CALL CG_IRIC_READ_REALSINGLE_F('rcorner' , ARAD(3) , IER)
      ELSE IF (SHAPE_TYPE == 5) THEN
C       non-standard
        TCR = 4

        CALL CG_IRIC_READ_FUNCTIONALSIZE_F('coords', NCS, IER)
        IF (NCS > MXCV) THEN
          ERR=-5
          CALL EROCUL (ERR,MXCV)
        ENDIF
        CALL CG_IRIC_READ_FUNCTIONAL_REALSINGLE_F('coords', scul,
     #    gcul, IER)
      ENDIF

      TC = TCR * 100 + MATERIAL * 10
      

C     -----------------------------------------------
C     Read data that corresponds to CV from CGNS file
C     -----------------------------------------------

      CALL CG_IRIC_READ_INTEGER_F(   'ali_type'      , ALI_TYPE, IER)
      CALL CG_IRIC_READ_REALSINGLE_F('srd_culvert'   , CLSRD, IER)
      CALL CG_IRIC_READ_REALSINGLE_F('l_cul'         , CLEN , IER)
      
      CALL CG_IRIC_READ_REALSINGLE_F('l_cul1'        , LCUL1 , IER)
      CALL CG_IRIC_READ_REALSINGLE_F('l_cul2'        , LCUL2 , IER)
      CALL CG_IRIC_READ_REALSINGLE_F('l_cul3'        , LCUL3 , IER)
      CALL CG_IRIC_READ_REALSINGLE_F('hw_d'          , HWD   , IER)
      
      CALL CG_IRIC_READ_REALSINGLE_F('us_invert'     , USINV, IER)
      CALL CG_IRIC_READ_REALSINGLE_F('ds_invert'     , DSINV, IER)
      
      if (ALI_TYPE == 3) THEN
        LMAX = LCUL1 + LCUL2 + LCUL3
        LMIN = LCUL2
        CLEN = LMAX - HWD / (2 * D) * (LMAX - LMIN)
      ENDIF

      BASEL=DSINV
      ZDROP=USINV-DSINV

      CALL cg_iric_read_integer_f('num_barrels', WEB, IER)
      WEB = WEB - 1

      WRITE(ERRIO,101) 'CV   EX01 ' // TRIM(FTOSTR(CLSRD)) // ',0,' //
     #  TRIM(FTOSTR(CLEN)) // ',' // TRIM(FTOSTR(DSINV)) // ',' //
     #  TRIM(FTOSTR(USINV)) // ',' // TRIM(ITOSTR(WEB+1))
      
C     -----------------------------------------------
C     Output CG record to stderr
C     -----------------------------------------------
      IF (SHAPE_TYPE == 0) THEN
C       Select type
        WRITE(ERRIO,101) 'CG        NOT PROPERLY SET. '
      ELSE IF (SHAPE_TYPE == 1) THEN
C       Box
        WRITE(ERRIO,101) 'CG        ' // TRIM(ITOSTR(TC)) // ',' //
     #    TRIM(FTOSTR(D)) // ',' // TRIM(FTOSTR(B))
      ELSE IF (SHAPE_TYPE == 2) THEN
C       circular
        WRITE(ERRIO,101) 'CG        ' // TRIM(ITOSTR(TC)) // ',' //
     #    TRIM(FTOSTR(D))
      ELSE IF (SHAPE_TYPE == 3) THEN
C       elliptical
        WRITE(ERRIO,101) 'CG        ' // TRIM(ITOSTR(TC)) // ',' //
     #    TRIM(FTOSTR(D)) // ',' // TRIM(FTOSTR(B))
      ELSE IF (SHAPE_TYPE == 4) THEN
C       pipe arch
        WRITE(ERRIO,101) 'CG        ' // TRIM(ITOSTR(TC)) // ',' //
     #    TRIM(FTOSTR(D)) // ',' // TRIM(FTOSTR(B)) // ',' //
     #    TRIM(FTOSTR(ARAD(1))) // ',' // TRIM(FTOSTR(ARAD(2))) //
     #    ',' // TRIM(FTOSTR(ARAD(3)))
      ELSE IF (SHAPE_TYPE == 5) THEN
C       non-standard
        CBUF = ''
        DO I = 0, NCS - 1
          IF (MOD(I, 6) == 0 .AND. I /= 0) THEN
            WRITE(ERRIO,101) '*CS       ' // TRIM(CBUF)
            CBUF = ''
          ENDIF
          
          CBUF = TRIM(ADJUSTL(CBUF)) // ' ' // TRIM(FTOSTR(SCUL(I+1)))
     #      // ',' // TRIM(FTOSTR(GCUL(I+1)))
        END DO
        IF (TRIM(CBUF) /= '') THEN
          WRITE(ERRIO,101) '*CS       ' // TRIM(ADJUSTL(CBUF))
        ENDIF
      ENDIF

      DR = B
      READY=READY+1

C     ------------------------------------------------
C     Read data that corresponds to *C1 from CGNS file
C     ------------------------------------------------

      CALL CG_IRIC_READ_INTEGER_F('c1_enable', C1_ENABLE, IER)
      IF (C1_ENABLE == 0) THEN
C       Use default values
      ELSE IF (C1_ENABLE == 1) THEN
C       Use CB12
        NHP=0
        CALL CG_IRIC_READ_REALSINGLE_F('cb12', CB12, IER)
        IF (CB12 > 0.98 .OR. CB12 < 0.85) THEN
          ERR=-13
          CALL EROCUL (ERR,I)
        ENDIF
        
        WRITE(ERRIO,101) '*C1       ' // TRIM(FTOSTR(CB12))
      ELSE IF (C1_ENABLE == 2) THEN
C       Use CP
        CALL CG_IRIC_READ_FUNCTIONALSIZE_F('cp', NHP, IER)
        IF (NHP > 4) NHP = 4
        CALL CG_IRIC_READ_FUNCTIONAL_REALSINGLE_F('cp', HP, CP, IER)

        ERR1=0
        ERR2=0
        DO I=1,NHP
          IF (CP(I) > 0.98 .OR.CP(I) < 0.65) ERR1=-11
          IF (I > 1) THEN
            IF(HP(I) <  HP(I-1)) ERR2=-12
          ENDIF
        END DO

        CBUF = ''
C 107    FORMAT(F6.2)
        DO I=1,NHP
          CBUF = TRIM(ADJUSTL(CBUF)) // ' ' // TRIM(FTOSTR(CP(I))) // 
     #      ',' // TRIM(FTOSTR(HP(I)))
        END DO
        WRITE(ERRIO,101) '*C1       ' // TRIM(ADJUSTL(CBUF))
        
        IF(ERR1.EQ.-11)THEN
          CALL EROCUL (ERR1,I)
          ERR=ERR1
        ENDIF
        IF(ERR2.EQ.-12)THEN
          CALL EROCUL (ERR2,I)
          ERR=ERR2
        ENDIF
      ENDIF
      
C     ------------------------------------------------
C     Read data that corresponds to *C5 from CGNS file
C     ------------------------------------------------

      CALL CG_IRIC_READ_INTEGER_F('c5_enable', C5_ENABLE, IER)

      IF (C5_ENABLE == 0) THEN
C       Use default values
      ELSE IF (C5_ENABLE == 1) THEN
C       Specify values
        CALL CG_IRIC_READ_REALSINGLE_F('c46', C46, IER)
        CALL CG_IRIC_READ_FUNCTIONALSIZE_F('c5', NC5, IER)
        IF (IER /= 0) NC5 = -1
        IF (NC5 > 4) NC5 = 4
        CALL CG_IRIC_READ_FUNCTIONAL_REALSINGLE_F('c5', H5, C5, IER)

        IF (C46 > 0.98 .OR. C46 < 0.65) THEN
          ERR=-16
          CALL EROCUL(ERR,I)
        ENDIF

        ERR1=0
        ERR2=0
        IF (NC5 > 0) THEN
          DO I=1,NC5
            IF (C5(I) > 0.75 .OR. C5(I) < 0.39) ERR1=-14
            IF(I > 1) THEN
              IF (H5(I) < H5(I-1)) ERR2=-15
            ENDIF
          END DO
        ELSE IF (NC5 == -1) THEN
          ERR1=-14
        ENDIF

        IF (ERR1 == -14) THEN
          CALL EROCUL (ERR1,I)
          ERR=ERR1
        ENDIF
        IF (ERR2 == -15) THEN
          CALL EROCUL (ERR2,I)
          ERR=ERR2
        ENDIF
        NH5=NC5
        
        IF (NC5 > 0) THEN
          CBUF = ''
          DO I=1,NC5
          CBUF = TRIM(ADJUSTL(CBUF)) // ' ' // TRIM(FTOSTR(C5(I)))
     #      // ',' // TRIM(FTOSTR(H5(I)))
          END DO
 109      FORMAT(A, F4.2, A, A)
          WRITE(ERRIO,109) '*C5       ', C46, ', ', TRIM(CBUF)
        ELSE
 110      FORMAT(A, F4.2)
          WRITE(ERRIO,110) '*C5       ', C46
        ENDIF
      ENDIF

C     ------------------------------------------------
C     Read data that corresponds to *CX from CGNS file
C     ------------------------------------------------

      CALL CG_IRIC_READ_FUNCTIONALSIZE_F(
     #  'tailwater_elevation', NTW, IER)
      IF (NTW > MAXTW) THEN
        ERR = -6
        CALL EROCUL (ERR, MAXTW)
      ENDIF
      CALL CG_IRIC_READ_FUNCTIONAL_REALSINGLE_F('tailwater_elevation',
     #  DUMMYIDS, H4TW, IER)
     
      CBUF = ''
      DO I=0, NTW-1
        IF (MOD(I, 6) == 0 .AND. I /= 0) THEN
          WRITE(ERRIO,101) '*CX       ' // TRIM(CBUF)
          CBUF = ''
        ENDIF

        IF (MOD(I, 6) /= 0) THEN
          CBUF = TRIM(CBUF) // ','
        ENDIF
        CBUF = TRIM(CBUF) // TRIM(FTOSTR(H4TW(I+1)))
      END DO

      IF (TRIM(CBUF) /= '') THEN
        WRITE(ERRIO,101) '*CX       ' // TRIM(ADJUSTL(CBUF))
      ENDIF
     
      DO I = 1, NTW
        H4TW(I) = H4TW(I) - BASEL
      END DO
      READY = READY + 100

C     ------------------------------------------------
C     Read data that corresponds to *CQ from CGNS file
C     ------------------------------------------------

      CALL CG_IRIC_READ_FUNCTIONALSIZE_F('discharges', NQ, IER)

      IF (NQ == 0) THEN
C       Discharge data omitted
        ERR1=-117
        CALL EROCUL (ERR1,I)
      ELSE IF (NQ > MAXQ) THEN
        ERR = -7
        CALL EROCUL (ERR, MAXQ)
      ELSE IF (NQ > 0) THEN
        CALL CG_IRIC_READ_FUNCTIONAL_REALSINGLE_F('discharges',
     #    DUMMYIDS, Q, IER)
        READY = READY + 1000

        CBUF = ''
        DO I=0, NQ-1
          IF (MOD(I, 6) == 0 .AND. I /= 0) THEN
            WRITE(ERRIO,101) '*CQ       ' // TRIM(CBUF)
            CBUF = ''
          ENDIF
          
          IF (MOD(I, 6) /= 0) THEN
            CBUF = TRIM(CBUF) // ','
          ENDIF
          CBUF = TRIM(CBUF) // TRIM(FTOSTR(Q(I+1)))
        END DO
        
        IF (TRIM(CBUF) /= '') THEN
          WRITE(ERRIO,101) '*CQ       ' // TRIM(ADJUSTL(CBUF))
        ENDIF
      ENDIF

C     ------------------------------------------------
C     Read data that corresponds to *CN from CGNS file
C     ------------------------------------------------

      CALL CG_IRIC_READ_REALSINGLE_F('n_cul' , N , IER)
      WRITE(ERRIO,101) '*CN       ' // TRIM(FTOSTR(N))

      READY=READY+10

C     -------------------------------------------------------
C     Read data that corresponds to *CC or *C3 from CGNS file
C     -------------------------------------------------------

      CALL CG_IRIC_READ_INTEGER_F('discharge_method', DISC_METHOD, IER)
      
C     The order of the IF statements was reversed from the original. 
C     At present, the use of *CC or *C3 is mutually exclusive in iRIC
C     however, that is not the case for manually coded input. If both
C     record types are coded, *C3 must precede *CC. The order was 
C     changed to ensure that occurs should there be a future change
C     to iRIC that permits both. 
      
      IF (DISC_METHOD == 1) THEN
C       Specify parameters (Use *C3 record)

C       Wingwalls
        CALL CG_IRIC_READ_INTEGER_F('ww_type', WW_TYPE, IER)
        IF (WW_TYPE == 0) THEN
C         None
          THETA = 0
        ELSE IF (WW_TYPE == 1) THEN
C         Equal Angled
          CALL CG_IRIC_READ_REALSINGLE_F('ww_theta', THETA0, IER)
          THETA = THETA0
          KWING = WINGWALL(THETA)
        ELSE IF (WW_TYPE == 2) THEN
C         Unequal Angled
          CALL CG_IRIC_READ_REALSINGLE_F('ww_theta1', THETA1, IER)
          CALL CG_IRIC_READ_REALSINGLE_F('ww_theta2', THETA2, IER)
          THETA = THETA1
          KWING1 = WINGWALL(THETA)
          THETA = THETA2
          KWING2 = WINGWALL(THETA)
          KWING = (KWING1 + KWING2) * 0.5
        END IF

C       Adjustment parameters

        CALL CG_IRIC_READ_INTEGER_F('inlet', INLET, IER)
        CALL CG_IRIC_READ_REALSINGLE_F('kr', TMP_KR, IER)
        CALL CG_IRIC_READ_REALSINGLE_F('kw', TMP_KW, IER)
        CALL CG_IRIC_READ_REALSINGLE_F('kproj', KPROJ, IER)

        IF (TMP_KR > 1.0) THEN
          KWR = TMP_KR
        ELSE IF (TMP_KW > 1.0) THEN
          KWR = TMP_KW
        END IF

C       output error messages if needed

        IF (KWING < 1.0)THEN
          ERR1=-104
          CALL EROCUL (ERR1,I)
          KWING=1.0
        ENDIF

        IF (TMP_KR < 1.0) THEN
C         rounding coefficient error
          ERR1=-102
          CALL EROCUL (ERR1,I)
        ENDIF

        IF (TMP_KW < 1.0) THEN
C         beveling coefficient error
          ERR1=-103
          CALL EROCUL (ERR1,I)
        ENDIF

        IF (KPROJ > 1.0) THEN
          ERR1=-106
          CALL EROCUL (ERR1,I)
          KPROJ=1.0
        ENDIF

        IF (INLET < 1 .OR. INLET > 4)THEN
          ERR1=-105
          CALL EROCUL (ERR1,I)
          INLET=1
        ENDIF
        
        IF (WW_TYPE == 2) THEN
          WRITE(ERRIO, 101) '*C3       ' // TRIM(FTOSTR(TMP_KR)) // ','
     #    // TRIM(FTOSTR(TMP_KW)) // ',(' // 
     #      TRIM(FTOSTR(THETA1)) // ',' // TRIM(FTOSTR(THETA2)) // '),'
     #    // TRIM(ITOSTR(INLET)) // ',' // TRIM(FTOSTR(KPROJ))
     
          WRITE(ERRIO, 101) '*  Unequal wing wall angles '
     #      // '(THETA1, THETA2) is shown here to'
          WRITE(ERRIO, 101) 'reflect actual culvert geometry, but'
     #      // ' format is not consistent with'
          WRITE(ERRIO, 101) 'format of the *C3 record in the CAP'
     #      // ' executable. See WRIR 98-4166 (p.22)'
          WRITE(ERRIO, 101) 'for details.'
     
        ELSE
          WRITE(ERRIO, 101) '*C3       ' // TRIM(FTOSTR(TMP_KR)) // ','
     #    // TRIM(FTOSTR(TMP_KW)) // ',' // TRIM(FTOSTR(THETA0)) // ','
     #    // TRIM(ITOSTR(INLET)) // ',' // TRIM(FTOSTR(KPROJ))
        END IF

      ELSE IF (DISC_METHOD == 0) THEN
C       Specify inlet geometry (Use *CC record)

        CALL CG_IRIC_READ_INTEGER_F('edg_type', EDGE_TYPE, IER)

        IF (EDGE_TYPE == 1) THEN
C         No Edging
          INLET = 1
        ELSE IF (EDGE_TYPE == 2) THEN
C         Rounding
          INLET = 1
          CALL CG_IRIC_READ_REALSINGLE_F('edg_rc', RNDD, IER)
        ELSE IF (EDGE_TYPE == 3) THEN
C         Beveled
          INLET = 1
          CALL CG_IRIC_READ_REALSINGLE_F('edg_wb', BEVD, IER)
          CALL CG_IRIC_READ_REALSINGLE_F('edg_ab', THETA0, IER)
          IF (THETA0 > 0) THEN
            THETA = THETA0
            KWING = WINGWALL(THETA)
          ENDIF
        ELSE IF (EDGE_TYPE == 4) THEN
C         Mitered end
          INLET = 2
        ELSE IF (EDGE_TYPE == 5) THEN
C         Bell mouth or tongue and groove
          INLET = 3
        ELSE IF (EDGE_TYPE == 6) THEN
C         Flared pipe
          INLET = 4
        ENDIF

        CALL cg_iric_read_integer_f('proj_type', PROJ_TYPE, IER)

        IF (PROJ_TYPE == 1) THEN
C         Flush
          LPRJCT = 0
        ELSE IF (PROJ_TYPE == 2) THEN
C         Equal Projection
          CALL CG_IRIC_READ_REALSINGLE_F('proj_l', LPRJCT, IER)
        ELSE IF (PROJ_TYPE == 3) THEN
C         Unequal Projection
          CALL CG_IRIC_READ_REALSINGLE_F('proj_l1', LPROJ1, IER)
          CALL CG_IRIC_READ_REALSINGLE_F('proj_l2', LPROJ2, IER)
          LPRJCT = (LPROJ1 + LPROJ2) * 0.5
        ENDIF
        
        WRITE(ERRIO,101) '*CC       ' // TRIM(FTOSTR(RNDD)) // ',' //
     #    TRIM(FTOSTR(BEVD)) // ',' // TRIM(FTOSTR(THETA0)) // ',' //
     #    TRIM(ITOSTR(INLET)) // ',' // TRIM(FTOSTR(LPRJCT))


      ENDIF

C     ------------------------------------------------
C     Read data that corresponds to *CF from CGNS file
C     ------------------------------------------------

      CALL CG_IRIC_READ_INTEGER_F('tflw', TFLW, IER)
      READY=READY+100000
      IF (TFLW == 5) THEN
        CALL CG_IRIC_READ_REALSINGLE_F('hflw', HFLW, IER)
      ENDIF

      IF (TFLW == 5) THEN
        WRITE(ERRIO,101) '*CF       ' // TRIM(ITOSTR(TFLW)) // ',' //
     #    TRIM(FTOSTR(HFLW))
      ELSE
        WRITE(ERRIO,101) '*CF       ' // TRIM(ITOSTR(TFLW))
      ENDIF

C 999   CONTINUE

C     Normalize INLET value
      INLET = IABS(INLET)
      IF (INLET <= 0) INLET=1

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

      IF (SI.EQ.1 .OR. SI.EQ.3) THEN
C       convert from m/s to ft/s units
        CALL CONVTOFTUNIT(NCS,SCUL,GCUL,B)
      ELSE
C       convert from inch to ft
        B=B/12.0
        D=D/12.0
      ENDIF

      DR=D  

C     Compute default C46 if no C46 is read from input data
      IF(C46.LE.0) C46 = QC46(TCR,BEVD,KWR,RND,THETA,INLET,KPROJ)
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
        READY=READY-1000
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
      ERR=0
      RETURN
      END
C
C=====RCVGEO_CGNS eof============================================================
