C-----ARCH bof===============================================================
C-----Purpose:
C       computes culvert properties for pipe arch sections
C     Programmed by: W Kirby original code from WSPRO
C     Date:
C     Modified by: JM Fulford
C
C
C
      SUBROUTINE ARCH
     I                 (DEP,B,D,ARAD,
     O                 ROUGH,AREA,CONV,TW,WP )
C
C-----Arguments:
      REAL B,D,ARAD(3)
      REAL DEP,AREA,WP,TW,CONV,ROUGH
C
C     NOTE -- DIMENSIONS DIMENS MUST BE GIVEN IN  INCHES ,  DEP IN  FEET.
C
      RISEQQ = D*12.0
      SPANQQ = B*12.0
      BRQQ = ARAD(1)
      TRQQ = ARAD(2)
      CRQQ = ARAD(3)
C
      Y3=DEP *12.0
      IF ( Y3 .LE. 0.     )  Y3 = 0.0001*RISEQQ
      IF ( Y3 .GT. RISEQQ )  Y3 =        RISEQQ
C
      SPHI1 = (SPANQQ/2.-CRQQ)/(BRQQ-CRQQ)
      CPHI1 = SQRT(1.-SPHI1**2)
      BDISQQ = CRQQ + (BRQQ-CRQQ)*(1.-CPHI1)
      TPHI2=(SPANQQ/2.-CRQQ)/(BDISQQ-(RISEQQ-TRQQ))
      PHI1=ATAN(SPHI1/CPHI1)
      PHI2=ATAN(TPHI2)
      Y1 = BRQQ-BRQQ*COS(PHI1)
      Y2 = BDISQQ +CRQQ*COS(PHI2)
C
      AREA3=0.
      AREA2=0.
      AREA1=0.
      WP1=0.
      WP2=0.
      WP3=0.
      TOP=0.0
      B1=2*BRQQ
      AK1=1./(BRQQ*BRQQ)
      RAD1=SQRT(B1*B1)
      B2=2*BDISQQ
      A2= CRQQ*CRQQ-BDISQQ*BDISQQ
      AK2=1./CRQQ**2
C      WRITE(*,*)'RAD2 SQRT ',B2,A2
      RAD2=SQRT(B2*B2-(4*A2*(-1.)))
      B3=2*(RISEQQ-TRQQ)
      A3=2*RISEQQ*TRQQ-RISEQQ*RISEQQ
      AK3=1./TRQQ**2
      RAD3=SQRT(B3*B3-(4*A3*(-1.)))
      IF(Y3.GE.Y2)GO TO 325
      Y2=Y3
      IF(Y3.GE.Y1)GO TO 345
      Y1=Y3
      IF(Y3.GT.0.0)GO TO 367
  325 X3=ABS(A3+B3*Y3-Y3*Y3)
      X33=A3+B3*Y2-Y2*Y2
      CALL MARQUE(Y3,Y2,B3,X3,X33,AK3,RAD3,
     $              XX1,ASUB1,XX2,ASUB2)
      AREA3=(ASUB1-ASUB2)/144.0*2.
      WP3 = 2.*TRQQ*(XX1-XX2)
  345 X2=A2+B2*Y2-Y2*Y2
      X22=A2+B2*Y1-Y1*Y1
      CALL MARQUE(Y2,Y1,B2,X2,X22,AK2,RAD2,
     $              XX1,ASUB1,XX2,ASUB2)
      ASUB3 = (SPANQQ/2.-CRQQ)* Y2-(SPANQQ/2.-CRQQ)*Y1
      AREA2= (ASUB1-ASUB2+ASUB3)/144.0*2.0
      WP2 = 2.*CRQQ*(XX1-XX2)
  367 X1=B1*Y1-Y1*Y1
      CALL MARQUE(Y1,0.,B1,X1,0.,AK1,RAD1,
     $              XX1,ASUB1,XX2,ASUB2)
      AREA1=(ASUB1-ASUB2)/144.0*2.
      WP1 = 2. *BRQQ*(XX1-XX2)
      AREA=AREA1+AREA2+AREA3
      WP  =(WP1+WP2+WP3)/12.0
      IF(Y3.GT.Y2)THEN
        TOP=2*SQRT(X3)
      ELSEIF(Y3.GT.Y1)THEN
        TOP=2*(SQRT (X2)+(SPANQQ/2.-CRQQ))
      ELSEIF(Y3.GT.0.0)THEN
        TOP=2*SQRT(X1)
      ENDIF
      TW=TOP/12.0
      CONV=1.49*AREA*(AREA/WP)**(2.0/3.0)/ROUGH
      IF( TW.LE.0. )  TW = 1E-8
C
      RETURN
      END
C
C=====ARCH eof===============================================================
C
C=====MARQUE bof=============================================================
C-----Purpose:
C       computes culvert properties for pipe arch sections
C     Programmed by: W Kirby original code from WSPRO
C     Date:
C     Modified by: JM Fulford
C
C
C
      SUBROUTINE MARQUE
     I                 (UL, LL, BB, UX, LX, AK,
     $                              RAD, XX1, ASUB1, XX2, ASUB2 )
C
C-----Arguments:
      REAL LL,LX,UL,BB,AK,RAD,XX1,ASUB1,XX2,ASUB2
C
      XX1 = (2.*UL-BB)/RAD
      IF(ABS(XX1).GT.1.) THEN
        XX1=SIGN(1.,XX1)
      ENDIF
      XX1= ASIN(XX1)
      ASUB1=((( -2*UL+BB)*SQRT(UX))*(-.25))+1./(2*AK)*XX1
      XX2=(2.*LL-BB)/RAD
      IF(ABS(XX2).GT.1.) THEN
        XX2=SIGN(1.  ,XX2)
      ENDIF
      XX2= ASIN(XX2)
      ASUB2=((((-2)*LL+BB)*SQRT(LX))*(-.25))+1./(2*AK)*XX2
C
      RETURN
      END
C
C=====MARQUE eof=============================================================
C
C=====CULPAD bof=============================================================
C-----Purpose:
C       computes culvert properties for pipe arch sections
C     Programmed by: W Kirby original code from WSPRO
C     Date:
C     Modified by: JM Fulford
C                  6.9.97 - uses arctyp code in material code wspro location
C     Modified by gfkoltun for 2021 version
C
      SUBROUTINE CULPAD
     I                 (TCR,D,B,ARAD)
C
C     CULPAD -- PIPE-ARCH DIMENSIONS -- APPROXIMATE
C
C        REGRESSION EQNS BASED ON DIMENSIONS TABULATED IN FHWA - CDS-4
C
C  PARAMETERS --
C     CMPA is corrugated metal pipe arch (no plates)
C     CMSPPA is corrugated metal structural plate pipe arch
C     RCP is reinforced concrete pipe
C
C     ARCTYP -- 
C     1 = RCP
C     2 = mitered CMSPPA (default CRR = 18")
C     3 = aluminum CMSPPA 
C     4 = CMPSPPA with corner radius (CR) <= 18"
C     5 = CMPSPPA with corner radius (CR) = 31"
C     6 = CMPSPPA with corner radius (CR) = 47"
C     7 = CMPA 
C     2,4 = CMP .LE. 18"CR,  5 = CMP-31",
C                 6 = CMP-47",  3 = ALUM. 31.8",   1 = R.C.P.
C     ARAD(1,2,3) = APPROX BOTTOM, TOP, CORNER RADII  =  OUTPUT.
C
C-----Arguments:
      INTEGER TCR
      REAL ARAD(3),D,B
C
C-----Local variables:
      INTEGER ARCTYP
      REAL SPAN,RISE
C
      SPAN=B*12.0
      RISE=D*12.0
C
      ARCTYP = (TCR - 300)/10 
C Added checks on computed/entered radii to warn user if they are outside the
C range of values used to develop the equations for estimating radii
C
C ARAD(1) = bottom radius, ARAD(2) = top radius, and ARAD(3) = corner radius
      IF(ARCTYP.EQ.2.OR.ARCTYP.EQ.4) THEN
        IF (ARAD(3).LT.1.0) THEN
            ARAD(3) = 18.
            IF (RISE.LT.55.) THEN
              ARAD(3) = 1.1072+0.1655*RISE+0.00195*(RISE-30.0417)**2.
              IF ((ARAD(3).LT.3.5).OR.(ARAD(3).GT.11.)) THEN
                  WRITE(*,300) ARAD(3),ARCTYP
                  CALL EROCUL(-121,0)
              ENDIF
            ENDIF
        ENDIF
        IF (ARAD(2).LT.1.0) THEN
            IF (RISE.LT.55.) THEN
              ARAD(2) = 0.594+(0.498*SPAN)
              IF ((ARAD(2).LT.10.06).OR.(ARAD(2).GT.42.63)) THEN
                  WRITE(*,200) ARAD(2),ARCTYP,10.06,42.63
                  CALL EROCUL(-120,0)
              ENDIF
            ELSE
              ARAD(2) = -0.183064+(0.504094*SPAN)
              IF ((ARAD(2).LT.36.8).OR.(ARAD(2).GT.85.1)) THEN
                  WRITE(*,200) ARAD(2),ARCTYP,36.8,85.1
                  CALL EROCUL(-120,0)
              ENDIF              
            ENDIF
        ENDIF
        IF (ARAD(1).LT.1.0) THEN 
            IF (RISE.LT.55.) THEN
              ARAD(1) = -4.1281+(2.2269*SPAN)-(0.6477*ABS(SPAN-42.))
              IF ((ARAD(1).LT.19.12).OR.(ARAD(1).GT.154.5)) THEN
                  WRITE(*,100) ARAD(1),ARCTYP,19.12,154.5
                  CALL EROCUL(-119,0)
              ENDIF 
            ELSE
              ARAD(1) = -641.6505-(16.04132*SPAN)+(34.36891*RISE)
              IF ((ARAD(1).LT.76.3).OR.(ARAD(1).GT.314.7)) THEN
                  WRITE(*,100) ARAD(1),ARCTYP,76.3,314.7
                  CALL EROCUL(-119,0)
              ENDIF 
            ENDIF
        ENDIF
      ELSE IF (ARCTYP.EQ.5) THEN
        ARAD(3) = 31.
        IF (ARAD(2).LT.1.0) ARAD(2) = -0.3673+0.505*SPAN
        IF (ARAD(1).LT.1.0) THEN
            ARAD(1) = -954.4966-(13.4897*SPAN)+(29.3799*RISE)
        ENDIF 
        IF ((ARAD(2).LT.80.1).OR.(ARAD(2).GT.124.)) THEN
           WRITE(*,200) ARAD(2),ARCTYP,80.1,124.
           CALL EROCUL(-120,0)
        ENDIF
        IF ((ARAD(1).LT.192.6).OR.(ARAD(1).GT.374.3)) THEN
          WRITE(*,100) ARAD(1),ARCTYP,192.6,374.3
          CALL EROCUL(-119,0)
        ENDIF
      ELSE IF (ARCTYP.EQ.6) THEN
        ARAD(3) = 47.
        IF (ARAD(2).LT.1.0) THEN
            ARAD(2) = -3.2662+0.5208*SPAN
        ENDIF
        IF (ARAD(1).LT.1.0) THEN
           ARAD(1) = -982.5186-(7.8061*SPAN)+(18.4433*RISE)
        ENDIF  
        IF ((ARAD(2).LT.122.5).OR.(ARAD(2).GT.188.2)) THEN
           WRITE(*,200) ARAD(2),ARCTYP,122.5,188.2
           CALL EROCUL(-120,0)
        ENDIF
        IF ((ARAD(1).LT.223.6).OR.(ARAD(1).GT.392.3)) THEN
          WRITE(*,100) ARAD(1),ARCTYP,223.6,392.3
        ENDIF
      ELSE IF (ARCTYP.EQ.3) THEN
        ARAD(3) = 31.75
        IF (ARAD(2).LT.1.0) THEN
            IF (RISE.LE.103.) THEN
              ARAD(2) = 28.8139+0.9352*SPAN-0.9034*RISE
              IF ((ARAD(2).LT.41.5).OR.(ARAD(2).GT.100.4)) THEN
                  WRITE(*,200) ARAD(2),ARCTYP,41.5,100.4
                  CALL EROCUL(-120,0)
              ENDIF              
            ELSE
              ARAD(2) = 26.4555+0.9241*SPAN-0.8427*RISE
              IF ((ARAD(2).LT.82.6).OR.(ARAD(2).GT.134.8)) THEN
                  WRITE(*,200) ARAD(2),ARCTYP,82.6,134.8
                  CALL EROCUL(-120,0)
              ENDIF
            ENDIF
        ENDIF
        IF (ARAD(1).LT.1.0) THEN
            IF (RISE.LE.103.) THEN
              ARAD(1) = -5.9001+1.4149*SPAN
              IF ((ARAD(1).LT.69.9).OR.(ARAD(1).GT.309.5)) THEN
                  WRITE(*,100) ARAD(1),ARCTYP,69.9,309.5
                  CALL EROCUL(-119,0)
              ENDIF 
            ELSE
              ARAD(1) = -676.9436-7.5948*SPAN+18.7191*RISE
              IF ((ARAD(1).LT.159.3).OR.(ARAD(1).GT.310.8)) THEN
                  WRITE(*,100) ARAD(1),ARCTYP,159.3,310.8
                  CALL EROCUL(-119,0)
              ENDIF 
            ENDIF
        ENDIF
      ELSE IF (ARCTYP.EQ.1) THEN
        IF (ARAD(3).LT.1.0) ARAD(3) = 0.5985+0.2431*RISE
        IF (ARAD(2).LT.1.0) ARAD(2) = 1.21+0.499*SPAN
        IF (ARAD(1).LT.1.0) THEN
            ARAD(1) = -60.13+2.106*SPAN+0.583*ABS(SPAN-95.)
        ENDIF
        IF ((ARAD(3).LT.4.03).OR.(ARAD(3).GT.26.87)) THEN
           WRITE(*,300) ARAD(3),ARCTYP,4.03,26.87
           CALL EROCUL(-121,0)
        ENDIF  
        IF ((ARAD(2).LT.10.62).OR.(ARAD(2).GT.85.63)) THEN
           WRITE(*,200) ARAD(2),ARCTYP,10.62,85.63
           CALL EROCUL(-120,0)
        ENDIF
        IF ((ARAD(1).LT.22.87).OR.(ARAD(1).GT.329.)) THEN
          WRITE(*,100) ARAD(1),ARCTYP,22.87,329.
          CALL EROCUL(-119,0)
        ENDIF
      ELSE IF (ARCTYP.EQ.7) THEN
        IF (ARAD(2).LT.1.0) ARAD(2) = 0.2203+0.499*SPAN
        IF (ARAD(3).LT.1.0) THEN
            IF (SPAN.LE.57.) THEN
              ARAD(3) = 0.7357+0.1831*SPAN
            ELSE IF (SPAN.GE.87.) THEN
              ARAD(3) = 3.2575+0.2201*SPAN
            ELSE
              WRITE(*,*)'WARNING - corner rounding should be specified'
              WRITE(*,*)'Corner radius assumed to be 21 inches.'
              ARAD(3) = 21.
            ENDIF
        ENDIF
        IF (ARAD(1).LT.1.0) THEN
          ARAD(1) = 8.8716+2.5464*SPAN-6.0731*ARAD(3)
        ENDIF
        IF ((ARAD(3).LT.3.5).OR.(ARAD(3).GT.41.)) THEN
           WRITE(*,300) ARAD(3),ARCTYP,3.5,41.
           CALL EROCUL(-121,0)
        ENDIF  
        IF ((ARAD(2).LT.8.625).OR.(ARAD(2).GT.85.)) THEN
           WRITE(*,200) ARAD(2),ARCTYP,8.625,85.
           CALL EROCUL(-120,0)
        ENDIF
        IF ((ARAD(1).LT.26.625).OR.(ARAD(1).GT.190.)) THEN
          WRITE(*,100) ARAD(1),ARCTYP,26.625,190.
          CALL EROCUL(-119,0)
        ENDIF
      ELSE
        WRITE(*,*)'Pipe archtype code invalid',ARCTYP
      ENDIF
100   FORMAT(' WARNING - Bottom radius (',F8.3,' in.) is outside calibra
     #tion range',/,'           for pipe-arch type = ',I1,' [',
     #F8.3,' - ',F8.3,']')
200   FORMAT(' WARNING - Top radius (',F8.3,' in.) is outside calibratio
     #n range',/,'           for pipe-arch type = ',I1,' [',
     #F8.3,' - ',F8.3,']')
300   FORMAT(' WARNING - Corner radius (',F8.3,' in.) is outside calibra
     #tion range',/,'           for pipe-arch type = ',I1,' [',
     #F8.3,' - ',F8.3,']')
C
      RETURN
      END
