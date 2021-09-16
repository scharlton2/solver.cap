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
C
C
      SUBROUTINE CULPAD
     I                 (TCR,D,B,ARAD)
C
C     CULPAD -- PIPE-ARCH DIMENSIONS -- APPROXIMATE
C
C        REGRESSION EQNS BASED ON DIMENSIONS TABULATED IN FHWA - CDS-4
C
C  PARAMETERS --
C     ARCTYP -- 2,4 = CMP .LE. 18"CR,  5 = CMP-31",
C                 6 = CMP-47",  3 = ALUM. 31.8",   1 = R.C.P.
C     ARAD(3,4,5) = APPROX BOTTOM, TOP, CORNER RADII  =  OUTPUT.
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
C
      IF(ARCTYP.EQ.2.OR.ARCTYP.EQ.4) THEN
        ARAD(3) = 18.
        IF (RISE.LT.55.) ARAD(3) = 1.141+0.205*RISE
        ARAD(2) = 0.594 + 0.498*SPAN
        ARAD(1) = 7.00 - 2.036*RISE + 2.741*SPAN
      ELSE IF (ARCTYP.EQ.5) THEN
        ARAD(3) = 31.
        ARAD(2) = -0.346 + 0.505*SPAN
        ARAD(1) = -956.6 + 29.39*RISE - 13.49*SPAN
      ELSE IF (ARCTYP.EQ.4) THEN
        ARAD(3) = 47.
        ARAD(2) = -3.27 + 0.521*SPAN
        ARAD(1) =  -982.3  + 18.44*RISE  - 7.805*SPAN
      ELSE IF (ARCTYP.EQ.3) THEN
        ARAD(3) = 31.8
        ARAD(2) = -0.696 + 0.522*SPAN
        ARAD(1)  =  363.3  - 9.639*RISE  +  5.398*SPAN
      ELSE IF (ARCTYP.EQ.1) THEN
        ARAD(3) = 0.598 + 0.243*RISE
        ARAD(2) = 1.21  +  0.499*SPAN
        ARAD(1)  =  -60.13  + 2.106*SPAN  +  0.583*ABS(SPAN-95.)
      ELSE
        WRITE(*,*)'Pipe archtype code invalid',ARCTYP
      ENDIF
C
      RETURN
      END
