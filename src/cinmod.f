C=====ARCIR bof=============================================================
C
C-----Purpose:
C       Computes geometric properties for circular section culverts
C     Programmed by:
C     Date:
C     Modified by: JM Fulford
C     Last modified:
C
      SUBROUTINE ARCIR
     I                (D,DEPTH,N,AREA,KONVEY,TW,WP)
C
C     + + + PURPOSE + + +
C
      USE iric
      USE iricio

      REAL N,KONVEY
C
      IF (ABS(DEPTH-D).LE.0.000001) THEN
        AREA=3.14159*0.25*D*D
        WP=3.14159*D
        TW=0.0
        HR=AREA/WP
      ELSE
        R=0.5*D
        ALP=ACOS((R-DEPTH)/R)
        AS=ALP*R*R
        AT=(R-DEPTH)*SQRT(R*R-(R-DEPTH)**2)
        AREA=AS-AT
        WP=2.0*R*ALP
        HR=AREA/WP
        TW=2.0*SQRT(R*R-(R-DEPTH)**2)
      ENDIF
      KONVEY=1.49*HR**(2.0/3.0)*AREA/N
C
      RETURN
      END
C
C=====ARCIR eof=============================================================
C
C=====ARBOX bof=============================================================
C
C-----Purpose:
C       Computes geometric properties for box culverts
C     Programmed by:
C     Date:
C     Modified by: JM Fulford
C     Last modified:
C
      SUBROUTINE ARBOX
     I                (DC,B,D,N,WEB,AREA,KONVEY,TW,WP)
C
C-----Arguments:
      INTEGER WEB
      REAL N,KONVEY
C
      RWEB=WEB
      WP=B+2.0*(1.0+RWEB)*DC
      IF (ABS(DC-D).LE.0.0001) WP=WP+B
      AREA=B*DC
      TW=B
      HR=AREA/WP
      KONVEY=1.49*HR**(2.0/3.0)*AREA/N
C
      RETURN
      END
C
C=====ARBOX eof=============================================================
C
C=====KINTER bof============================================================
C
C-----Purpose:
C       Linearly interpolates conveyances, areas, alpha, for the
C       approach section from the table of approach section geometries.
C     Programmed by: JM Fulford
C     Date:
C     Modified by:
C     Last modified:
C
      SUBROUTINE KINTER
     I                 (H,
     O                  AREA,KONVEY,WIDTH,ALPHA,ERR)
C
C-----Arguments:
      INTEGER ERR
      REAL H,AREA,KONVEY,WIDTH,ALPHA
C
C-----Argument defintions:
C     H      - water surface elevation in relation to base datum
C     AREA   - approach section area
C     KONVEY - approach section conveyance
C     WIDTH  - approach section top width
C     ALPHA  - velocity coefficient
C     ERR    - err=0, no error; err=1, failed to interpolate
C
C-----Module data:
      INCLUDE 'APPROC.INC'
C
C-----Local variables:
      INTEGER J,FLG
      REAL M
C
      ERR=0
      J=0
      FLG=0
      AREA=0.0
10    CONTINUE
        J=J+1
        IF (H.GE.W1(J).AND.H.LE.W1(J+1)) FLG=1
      IF(FLG.NE.1.AND.J.LT.NQW-1) GO TO 10
      IF(FLG.EQ.1)THEN
        M=(H-W1(J))/(W1(J+1)-W1(J))
        WIDTH=B1(J)+M*(B1(J+1)-B1(J))
        AREA=0.5*(B1(J)+WIDTH)*(H-W1(J))+A1(J)
Cwrite        WRITE(*,*)'M, AREA, A1(J), W1(J), W1(J+1), B1(J), J'
Cwrite        WRITE(*,*)M,AREA,A1(J),W1(J),W1(J+1),B1(J),J
C        AREA=A1(J)+M*(A1(J+1)-A1(J))
C        IF(AREA.LE.1.0)THEN
C          WRITE(*,*)'KINTER AREA<=1 ',A1(J),A1(J+1),H,M,J
C        ENDIF
      ENDIF
      IF (AREA.GE.0.0.AND.H.LT.W1(J+1)) THEN
C        KONVEY=K1(J)+M*(K1(J+1)-K1(J))
        ALPHA=APH1(J)+M*(APH1(J+1)-APH1(J))
        WPER=WP1(J)+M*(WP1(J+1)-WP1(J))
        ESTN=0
        ESTN1=0
        IF(A1(J).GT.0)
     #    ESTN=K1(J)*WP1(J)**0.66667/(A1(J)**1.66667)
        IF(A1(J+1).GT.0)
     #    ESTN1=K1(J+1)*WP1(J+1)**0.66667/(A1(J+1)**1.66667)
        ESTN=ESTN+M*(ESTN1-ESTN)
        KONVEY=ESTN*AREA**1.66667/(WPER**0.66667)
      ELSE IF(H.LE.W1(J+1).AND.AREA.GT.0)THEN
          AREA=A1(J+1)
          KONVEY=K1(J+1)
          WIDTH=B1(J+1)
      ELSE
        ERR=1
      ENDIF
C
      RETURN
      END
C
C=====KINTER eof============================================================
C
C=====CULINT bof============================================================
C
C     Purpose:
C       Linearly interpolate values from culvert geometric properties
C     Programmed by: JM Fulford
C     Date:
C     Modified by:
C     Last modified:
C
      SUBROUTINE CULINT
     I                 (HS4,
     O                  AREA,KONVEY,TOPW,ERR)
C
C
C-----Arguments:
      INTEGER ERR
      REAL HS4,AREA,KONVEY,TOPW
C
C-----Argument definitons:
C     HS4    - water surface elevation
C     AREA   - culvert area
C     KONVEY - culvert conveyance
C     TOPW   - top width
C     ERR    - err=0, no error;  err=2, failed to interpolate
C
C-----Module data:
      INCLUDE 'CULVRT.INC'
C
C-----Local variables:
      INTEGER FLG,K,KLOOP,J
      REAL M,WPER,ESTN,ESTN1
C
      ERR=0
      M=0.0
      FLG=0
      AREA=0.0
      KONVEY=0.0
      TOPW=0.0
      KLOOP=TBSIZE-5
      IF(HS4.GT.0.0) THEN
        J=-4
10      CONTINUE
          J=J+5
          IF (HS4.GE.CULD(J).AND.HS4.LE.CULD(J+5)) FLG=1
        IF(FLG.NE.1.AND.J.LT.KLOOP) GO TO 10
C        IF(FLG.NE.1.AND.J.LT.21) GO TO 10
        IF(FLG.NE.1)THEN
C          K=25
C          K=50
           K=TBSIZE-1
          IF(ABS(HS4-CULD(J+5))/HS4.GE.0.001) THEN
            ERR=2
          ENDIF
        ELSE
          FLG=0
          N=J+5
          K=J-1
15        CONTINUE
            K=K+1
            IF (HS4.GE.CULD(K).AND.HS4.LE.CULD(K+1)) FLG=1
          IF(FLG.NE.1.AND.K.LT.(N-1)) GO TO 15
        ENDIF
        IF(ERR.EQ.0.AND.HS4.LT.CULD(K+1))THEN
          M=(HS4-CULD(K))/(CULD(K+1)-CULD(K))
c          AREA=CULA(K)+M*(CULA(K+1)-CULA(K))
C          KONVEY=CULK(K)+M*(CULK(K+1)-CULK(K))
          TOPW=CULT(K)+M*(CULT(K+1)-CULT(K))
          WPER=CULWP(K)+M*(CULWP(K+1)-CULWP(K))
          AREA=0.5*(CULT(K)+TOPW)*(HS4-CULD(K))+CULA(K)
          ESTN=0
          ESTN1=0
          IF(CULA(K).GT.0)
     #      ESTN=CULK(K)*CULWP(K)**0.66667/(CULA(K)**1.66667)
          IF(CULA(K+1).GT.0)
     #      ESTN1=CULK(K+1)*CULWP(K+1)**0.66667/(CULA(K+1)**1.66667)
          ESTN=ESTN+M*(ESTN1-ESTN)
          KONVEY=ESTN*AREA**1.66667/(WPER**0.66667)
        ELSE IF(HS4.LE.CULD(K+1).AND.ERR.EQ.0)THEN
          AREA=CULA(K+1)
          KONVEY=CULK(K+1)
          TOPW=CULT(K+1)
        ENDIF
      ELSE
        KONVEY=0.0
        AREA=0.0
        TOPW=0.0
        ERR=2
      ENDIF
C
      RETURN
      END
C
C=====CULINT eof===========================================================
C
C
C=====QCRIT bof===============================================================
C-----Purpose:
C       Function to compute critical flow for iteration subroutine (RTMIA)
C     Programmed by: JM Fulford
C     Date:
C     Modified by:
C     Last modified:
C
      REAL FUNCTION QCRIT(DEPTH,ERR)
C
C-----Arguments:
      INTEGER ERR
      REAL DEPTH,KONVEY
C
C-----Module data:
      INCLUDE 'FCNCN2.INC'
C
      QCRIT = -1.0*CQ
      CALL CULINT(DEPTH,AREA,KONVEY,TOPW,ERR)
      IF(ERR.EQ.0.AND.AREA.GT.0.0) THEN
        QCRIT = (AREA**1.5)*SQRT(32.2/TOPW) - CQ
      ELSE IF(DEPTH.GT.0.0) THEN
        WRITE (*,20)
   20   FORMAT(1X,'FAILED IN CULINT LOOKUP IN QCRIT SUBRT')
      ENDIF
      ERR=0
C
      RETURN
      END
C
C=====QCRIT eof===============================================================
C
C=====RTMIA bof===============================================================
C
C-----Purpose:
C       Uses Brent's method to find the root of a function
C       known to lie between XLI and XRI. See Numerical
C       Recipes for details.
C     Programmed by: JM Fulford
C     Date:
C     Modified by:
C     Last modified:
C
      SUBROUTINE RTMIA
     I                (X,F
     O                 ,FCT,XLI,XRI,TOL,IEND,IER)
C
C-----Arguments:
      INTEGER IEND,IER
      REAL X,F,XLI,XRI,TOL,FCT
C
C-----Local variables:
      INTEGER I,ERR
      REAL XA,XB,XC,XD,XE,XM,FA,FB,FC,P,Q,R,S,TOL1,EPS
C
      PARAMETER (EPS=3.E-8)
C
      EXTERNAL FCT
C
      IER=0
      XA=XLI
      XB=XRI
      ERR=0
      FA=FCT(XA,ERR)
      IF(ERR.EQ.0) FB=FCT(XB,ERR)
      IF(ERR.EQ.0) THEN
      IF(FB*FA.LE.0.0)THEN
        FC=FB
        DO 11 I=1,IEND
          IF(FB*FC.GT.0.0)THEN
            XC=XA
            FC=FA
            XD=XB-XA
            XE=XD
          ENDIF
          IF(ABS(FC).LT.ABS(FB))THEN
            XA=XB
            XB=XC
            XC=XA
            FA=FB
            FB=FC
            FC=FA
          ENDIF
          TOL1=2.*EPS*ABS(XB)+0.5*TOL
          XM=.5*(XC-XB)
          IF(ABS(XM).LE.TOL1.OR.FB.EQ.0.0)THEN
            F=FB
            X=XB
            RETURN
C*****
          ENDIF
          IF(ABS(XE).GE.TOL1.AND.ABS(FA).GT.ABS(FB)) THEN
            S=FB/FA
            IF(XA.EQ.XC)THEN
              P=2.*XM*S
              Q=1.-S
            ELSE
              Q=FA/FC
              R=FB/FC
              P=S*(2.*XM*Q*(Q-R)-(XB-XA)*(R-1.))
              Q=(Q-1.)*(R-1.)*(S-1.)
            ENDIF
            IF(P.GT.0.) Q=-Q
            P=ABS(P)
            IF(2.*P.LT.MIN(3.*XM*Q-ABS(TOL1*Q),ABS(XE*Q)))THEN
              XE=XD
              XD=P/Q
            ELSE
              XD=XM
              XE=XD
            ENDIF
          ELSE
            XD=XM
            XE=XD
          ENDIF
          XA=XB
          FA=FB
          IF(ABS(XD).GT.TOL1)THEN
            XB=XB+XD
          ELSE
            XB=XB+SIGN(TOL1,XM)
          ENDIF
          FB=FCT(XB,IER)
 11     CONTINUE
        IER=-1
        X=XB
        F=FB
      ELSE
C       error, upper & lower bounds did not bracket root
Cwrite        WRITE(*,*)'FA, XA, FB, XB ',FA, XA, FB, XB,ABS(FA),ABS(FB)
        IER=-2
C       if ier=-2 nearer upper bound, if ier=-3 nearer lower bound
        IF(ABS(FA).LT.ABS(FB)) IER=-3
      ENDIF
      ELSE
        IER=ERR
        WRITE(*,*)'RTMIA HAS ERROR',ERR
      ENDIF
C
      RETURN
      END
C
C=====RTMIA eof===============================================================
C
C=====HARDCPY bof===========================================================
C
C-----Purpose:
C       Prints out results of culvert program in an 80 character line by
C       80 line format.  The information is similar to the old 132 char.
C       line format but is organized differently.
C     Programmed by: JM Fulford
C     Date:
C     Modified by: jmfulford
C     Last modified:  9.03.1999 altered FLGERR array handling and corrected
C                     print statement
C     Modified by gfkoltun for 2021 version
C
      SUBROUTINE HARDCPY (PUNIT,PNO,HTITLE,CGNSOUT)

      use iric
      use iricio
C
C-----Arguments:
      INTEGER PUNIT,PNO,CGNSOUT
      CHARACTER*80 HTITLE
C
C-----Module data:
      INCLUDE 'CULFLW.INC'
C
C-----Parameters:
      INTEGER NPAGE
      PARAMETER (NPAGE=60)
C
C-----Local variables:
      REAL K2,K3,KA,H2,H3
      INTEGER ERR,IQ,PAGE,IPAGE,IBEGIN,IEND,ILEFT,I,J,WARN,FLGERR(9)
     #        ,NLINES,PNOH
      
      DOUBLE PRECISION, allocatable, dimension(:,:)
     #  :: DISCHARGE
      DOUBLE PRECISION, allocatable, dimension(:,:)
     #  :: EXITELEV
      DOUBLE PRECISION, allocatable, dimension(:,:)
     #  :: WSE_APPR,WSE_INLET,WSE_OUTLET,C_DC
      DOUBLE PRECISION :: TIME
      INTEGER ITW,IER
C
C-----Formats
  200 FORMAT (/,9X,'Discharge  Flow     Water Surface Elevations'
     # ,' (feet) Critical'
     #       ,2X,'Error'
     #       ,/,3X,'no.',5X,'(cfs)',4X,'type',5X,'appr.',3X,'inlet',3X,
     #        'outlet',3X,'exit     Dc       code@')
  210 FORMAT (2X,I3,3X,F10.1,2X,I2,5X,4F8.2,2X,F7.1,2X,I3)
  220 FORMAT (/,12X,' Fall (ft)     Losses (ft)   ',4X,'Appr. Section',
c  220 FORMAT (/,12X,'+- Fall --+ +---- Losses ---+',4X,'Appr. Section',
     #       2X,'Control Section',/,2X,'no.',3X,'C   entry  eff.  entry'
     #        ,'(1-2) (2-3)',4X,'VH  alph   F',4X,'energy    F')
  230 FORMAT (1X,I3,3X,F4.2,5(1X,F5.2),3X,2(F4.2,1X),F5.2,1X,F8.2,1X,
     #        F5.2)
  240 FORMAT('_______________________________________________________'
     #      ,'_________________________',/,'Abrevs. used: appr.-appro'
     #      ,'ach  C-discharge coefficient  eff.-effective',/,'VH-vel'
     #     ,'ocity head  alph-velocity coefficient  n-Manning''s roug'
     #      ,'hness coef.',/,'energy-specific energy  F-Froude number'
     #      ,//,'Note: For flow types 1-3, the C values shown were '
     #      ,'adjusted for channel',/,'contraction, if necessary, ' 
     #      ,'according to fig. 19 in TWRI 3-A3 (p. 38).',//
     #      ,'      @Error code'     
     #      ,'s:  -1,1-7 fatal error; 8-15 warning; 0 no error')
  250 FORMAT(//,10X,'---------Warning Messages-------------',/)
C     format statements for m-s unit output
  300 FORMAT (/,9X,'Discharge  Flow     Water Surface Elevations'
     # ,'(meter) Critical'
     #       ,2X,'Error'
     #       ,/,3X,'no.',5X,'(cms)',4X,'type',5X,'appr.',3X,'inlet',3X,
     #        'outlet',3X,'exit     Dc       code@')
  310 FORMAT (2X,I3,2X,F11.2,2X,I2,4X,4F8.3,2X,F7.2,2X,I3)
  320 FORMAT (/,12X,' Fall (m)      Losses (m)    ',4X,'Appr. Section',
     #       2X,'Control Section',/,2X,'no.',3X,'C   entry  eff.  entry'
     #        ,'(1-2) (2-3)',4X,'VH  alph   F',4X,'energy    F')
  330 FORMAT (1X,I3,3X,F4.2,5(F6.3),2X,F5.3,1X,F4.2,1X,F5.2,1X,F8.2,1X,
     #        F5.2)
C
      IQ=1
      NLINES=(NPAGE-20)/2
      PAGE=INUM/NLINES
      IF(MOD(INUM,NLINES).NE.0)PAGE=PAGE+1
      IBEGIN=0
      IEND=0
C     ******* gfk - temporarily added line below to allocate arrays ****
      CGNSOUT=1
      
      IF (CGNSOUT.EQ.1) THEN
        ALLOCATE(DISCHARGE(NQ,NTW))
        ALLOCATE(EXITELEV(NQ,NTW))
        ALLOCATE(WSE_APPR(NQ,NTW))
        ALLOCATE(WSE_INLET(NQ,NTW))
        ALLOCATE(WSE_OUTLET(NQ,NTW))
        ALLOCATE(C_DC(NQ,NTW))
        
        DO I=1,NQ
          DO J=1,NTW
            DISCHARGE(I,J)=Q(I)
            EXITELEV(I,J)=H4TW(J)
          END DO
        END DO
        
        WSE_APPR = 0
        WSE_INLET = 0
        WSE_OUTLET = 0
        C_DC = 0
      ENDIF
      
      DO 10 IPAGE=1,PAGE
        IBEGIN=1+IEND
        ILEFT=INUM-(IPAGE*NLINES)
        IF(ILEFT.GE.0) THEN
          IEND=IPAGE*NLINES
        ELSE
          IEND=(IPAGE-1)*NLINES+MOD(INUM,NLINES)
        ENDIF
        PNOH=IPAGE+PNO
        CALL HEADER(PUNIT,PNOH,HTITLE)
C---------write head2
C        WRITE(PUNIT,*)IBEGIN,IEND
        IF(SIC.EQ.0.OR.SIC.EQ.3) THEN
          WRITE(PUNIT,200)
        ELSE
          WRITE(PUNIT,300)
        ENDIF
        DO 15 I=IBEGIN,IEND
          IQ=CNTRS(I)
          ITW=H4TWID(I)

          IF(D2(I).EQ.-1)THEN
            H2=1.0E10
          ELSE
            H2=D2(I)+BASEL+ZDROP
          ENDIF
          IF(D3A(I).EQ.-1)THEN
            H3=1.0E10
          ELSE IF(TYPE(I).EQ.5.AND.D3A(I)+BASEL.GE.H4E(I)) THEN
            H3=H4E(I)
          ELSE
            H3=D3A(I)+BASEL
          ENDIF
          IF(SIC.EQ.1.OR.SIC.EQ.2)THEN
            WRITE(PUNIT,310)I,ft3_to_m3(Q(IQ)),TYPE(I),ft_to_m(H1E(I)),
     #        ft_to_m(H2),ft_to_m(H3),ft_to_m(H4E(I)),
     #        ft_to_m(CULCRTD(I)),ERRCOD(I)
            WSE_APPR(IQ,ITW) = ft_to_m(H1E(I))
            WSE_INLET(IQ,ITW) = ft_to_m(H2)
            WSE_OUTLET(IQ,ITW) = ft_to_m(H3)
            C_DC(IQ,ITW) = ft_to_m(CULCRTD(I))
          ELSE
            WRITE(PUNIT,210)I,Q(IQ),TYPE(I),H1E(I),H2,H3,H4E(I)
     #       ,CULCRTD(I),ERRCOD(I)
            WSE_APPR(IQ,ITW) = H1E(I)
            WSE_INLET(IQ,ITW) = H2
            WSE_OUTLET(IQ,ITW) = H3
            C_DC(IQ,ITW) = CULCRTD(I)
          ENDIF
 15     CONTINUE
C-------write head2
        IF(SIC.EQ.0.OR.SIC.EQ.3) THEN
          WRITE(PUNIT,220)
        ELSE
          WRITE(PUNIT,320)
        ENDIF
        DO 20 I=IBEGIN,IEND
          PFALL1=1.0E10
          PFALL2=1.0E10
          PENRGY=1.0E10
          PF2   =1.0E10
          PHENT =1.0E10
          PH12  =1.0E10
          PH23  =1.0E10
          IQ=CNTRS(I)
          IF(ERRCOD(I).EQ.0.OR.ERRCOD(I).GE.8.AND.ERRCOD(I).NE.16) THEN
            XI=H1E(I)-BASEL
            CALL KINTER (XI,AA,KA,BA,APA,ERR)
Cwrite            WRITE(*,*)'KINTER IN HARDCPY',XI,AA,KA,BA,D,ERR
            IF(ERR.EQ.0)THEN
              PF1 = (Q(IQ)/AA) / SQRT(32.2*(AA/BA))
              PALPH = APA
              PHV1 = APA*(Q(IQ)/AA)**2.0/64.4
            ELSE
              WRITE(*,*)'Depth: ',XI,' OUTSIDE TABLED PROPERTIES FOR A'
     #        ,'PPROACH'
              PF1=0.
              PALPH=0.
              PHV1=0.
            ENDIF
C
            IF(TYPE(I).EQ.1)THEN
              CALL CULINT (D2(I),A2,K2,TW,ERR)
              IF(ERR.NE.0) THEN
Cwrite                WRITE(*,*)'ERR CULINT HARDCPY',ERR,D2(I),A2,KA,TW
              ENDIF
              PENRGY = D2(I)+ BASEL+ZDROP+(Q(IQ)/A2)**2.0 / 64.4
              PF2 = (Q(IQ)/A2) / SQRT(32.2*(A2/TW))
              PHENT =((1/(PKON(I)*PKON(I)))-1)*(Q(IQ)/A2)**2.0/64.4
              PH12 = LW*Q(IQ)*Q(IQ)/(KA*K2)
              PFALL1 = XI -ZDROP- D2(I)
              PFALL2 = PFALL1+ PHV1 - PH12
            ELSE IF(TYPE(I).EQ.2.OR.TYPE(I).EQ.3.OR.TYPE(I).EQ.34)THEN
              CALL CULINT(D2(I),A2,K2,TW,ERR)
              CALL CULINT(D3A(I),A3,K3,TW,ERR)
Cwrite              WRITE(*,*)A2,K2,A3,K3,KA,TW,PKON(I)
              PENRGY = D3A(I) + BASEL + (Q(IQ)/A3)**2.0/64.4
              PHENT = (1/(PKON(I)*PKON(I))-1)*(Q(IQ)/A3)**2.0/64.4
              PH12 = LW * Q(IQ)* Q(IQ)/ (KA*K2)
              PFALL1 = XI - D3A(I)              
              IF(TYPE(I).NE.34) THEN
                PH23 = L * Q(IQ)*Q(IQ)/ (K2*K3)
                PFALL2 = PFALL1 + PHV1 - PH12 - PH23 
                IF(TW.GT.0) THEN
                  PF2 = (Q(IQ)/A3)/SQRT(32.2*A3/TW)
                ELSE
                  PF2 = (Q(IQ)/A3)/SQRT(32.2*D)
                ENDIF
              ENDIF
            ELSE IF(TYPE(I).LT.7.OR.TYPE(I).EQ.43)THEN
              CALL CULINT(D,A2,K2,TW,ERR)
Cwrite              WRITE(*,*)'HIGH',PKON(I),D,A2,K2,TW
              PHENT = (1/(PKON(I)*PKON(I))-1)*(Q(IQ)/A2)**2.0/64.4
              PH12 = LW*Q(IQ)*Q(IQ)/(KA*KA)
              IF (TYPE(I).EQ.6.OR.TYPE(I).EQ.4.OR.TYPE(I).EQ.43)
     #             PH23 = L*Q(IQ)*Q(IQ)/(K2*K2)
              IF (TYPE(I).EQ.4.OR.TYPE(I).EQ.43) THEN
                PFALL1 = H1E(I) - H4E(I)
                PFALL2 = PFALL1 + PHV1 - PH12 -PH23
              ELSE IF (TYPE(I).EQ.5) THEN
                PFALL2 = XI -ZDROP
              ELSE IF (TYPE(I).EQ.6) THEN
                PFALL2 = ((Q(IQ)/(A2*PKON(I)))**2.0)/64.4
              ENDIF
            ENDIF
            IF (PFALL2.LT.0.0) PFALL2 = 1.0E10
            IF(SIC.EQ.1.OR.SIC.EQ.2)THEN
              PFALL1=ft_to_m(PFALL1)
              PFALL2=ft_to_m(PFALL2)
              PHENT=ft_to_m(PHENT)
              PH12=ft_to_m(PH12)
              PH23=ft_to_m(PH23)
              PHV1=ft_to_m(PHV1)
              PENRGY=ft_to_m(PENRGY)
            ENDIF
            IF (SIC.EQ.1.OR.SIC.EQ.2) THEN
              WRITE(PUNIT,330)I,PKON(I),PFALL1,PFALL2,PHENT,PH12,PH23,
     #         PHV1,PALPH,PF1,PENRGY,PF2
            ELSE
              WRITE(PUNIT,230)I,PKON(I),PFALL1,PFALL2,PHENT,PH12,PH23,
     #         PHV1,PALPH,PF1,PENRGY,PF2
            ENDIF
          ELSE
            CALL ERRPRNT(I,PUNIT,ERRCOD(I))
          ENDIF
 20      CONTINUE
C------write footer
        WRITE(PUNIT,240)
 10   CONTINUE

      IF (CGNSOUT.EQ.1) THEN
        CALL CG_IRIC_WRITE_GRID2D_COORDS(FID,NQ,NTW,
     #    DISCHARGE,EXITELEV,IER)
        TIME=0
        CALL CG_IRIC_WRITE_SOL_TIME(FID,TIME,IER)
        CALL CG_IRIC_WRITE_SOL_NODE_REAL(FID,'wse_approach',
     #    WSE_APPR,IER)
        CALL CG_IRIC_WRITE_SOL_NODE_REAL(FID,'wse_inlet',
     #    WSE_INLET,IER)
        CALL CG_IRIC_WRITE_SOL_NODE_REAL(FID,'wse_outlet',
     #    WSE_OUTLET,IER)
        CALL CG_IRIC_WRITE_SOL_NODE_REAL(FID,'critical_dc',
     #    C_DC,IER)
          
        DEALLOCATE(DISCHARGE, EXITELEV)
        DEALLOCATE(WSE_APPR,WSE_INLET,WSE_OUTLET,C_DC)
      ENDIF
C
C-----print warning messages at end of output
      DO 30 I=1,9
        FLGERR(I)=0
 30   CONTINUE
      WRITE(PUNIT,250)
      DO 35 I=1,INUM
        WARN=ERRCOD(I)-7
        IF(WARN.GE.1.AND.WARN.NE.16) THEN
          IF(FLGERR(WARN).EQ.0) THEN
            CALL ERRPRNT(I,PUNIT,ERRCOD(I))
            FLGERR(WARN)=1
          ENDIF
        ENDIF
 35   CONTINUE
C
      RETURN
      END
C
C=====HARDCPY eof===========================================================
C
C=====ERRPRNT bof===========================================================
C
C-----Purpose:
C     Programmed by: JM Fulford
C       Prints out the error messages for various errcodes
C     Date:
C     Modified by:
C     Last modified:
C
C
      SUBROUTINE ERRPRNT(I,PUNIT,ERRCD)
C
C
C-----Arguments:
      INTEGER PUNIT,ERRCD,I
C
C-----Format:
   98 FORMAT (' ',I3,3X,'Root not bracketed in approach section')
   99 FORMAT (' ',I3,3X,'Root did not close to tolerance')
  100 FORMAT (' ',I3,3X,'Root not bracketed')
  101 FORMAT (' ',I3,3X,'FAILED INTERPOLATION, -- for approach'
     #        ,' section properties')
  102 FORMAT (' ',I3,3X,'FAILED INTERPOLATION, -- for culvert'
     #        ,' properties')
  103 FORMAT (' ',I3,3X,'FAILED INTERPOLATION, -- for discharge'
     #       ,' coefficients')
  104 FORMAT (' ',I3,3X,'No solution type one flow','---SUPERCRI'
     #       ,'TICAL FLOW at approach section')
  105 FORMAT (' ',I3,3X,'No solution type 2 or 3 flow','---SUPERCRI'
     #       ,'TICAL FLOW at approach section')
  106 FORMAT (' ',I3,3X,'DISCHARGE out of range of critical discharges')
  107 FORMAT (' ',I3,3X,'No solution type five flow')
  108 FORMAT (' ',I3,3X,'WARNING -- flow type 1 & 3 in barrel',
     #        ' used type 1')
  109 FORMAT (' ',I3,3X,'WARNING -- No solution for type 3 inlet -- ',
     #        'assumed normal flow depth in inlet')
  110 FORMAT (' ',I3,3X,'WARNING -- linearily interpolated between',
     #        ' high head flows',/,18X,'and type 2 instead of type 3')
  111 FORMAT (' ',I3,3X,'WARNING -- linearily interpolated',
     #        ' between high head flows',/,18X,
     #        'and low head flows')
  112 FORMAT (' ',I3,3X,'WARNING -- flow did not meet high',
     #        ' head criteria for type 5')
  113 FORMAT (' ',I3,3X,'WARNING -- culvert is flowing full',
     #        '  and part full')
  114 FORMAT (' ',I3,3X,'WARNING -- not high head flow')
  115 FORMAT (' ',I3,3X,'WARNING -- h/D exceeds 6.5 for type 6')
  116 FORMAT (' ',I3,3X,'ERROR -- in type 6 flow computation')
  120 FORMAT (' ',I3,3X,'WARNING -- error detected in computation')
C
      IF(ERRCD.LE.-2.AND.ERRCD.GT.-4) THEN
        WRITE(PUNIT,100) I
      ELSE IF(ERRCD.EQ.-4)THEN
        WRITE(PUNIT,98) I
      ELSE IF(ERRCD.EQ.-1)THEN
        WRITE(PUNIT,99) I
      ELSE IF(ERRCD.EQ.1)THEN
        WRITE(PUNIT,101) I
      ELSE IF(ERRCD.EQ.2)THEN
        WRITE(PUNIT,102) I
      ELSE IF(ERRCD.EQ.3) THEN
        WRITE(PUNIT,103) I
      ELSE IF(ERRCD.EQ.4) THEN
        WRITE(PUNIT,104) I
      ELSE IF(ERRCD.EQ.5) THEN
        WRITE(PUNIT,105) I
      ELSE IF(ERRCD.EQ.6) THEN
        WRITE(PUNIT,106) I
      ELSE IF (ERRCD.EQ.7) THEN
        WRITE(PUNIT,107) ERRCD
      ELSE IF (ERRCD.EQ.8) THEN
        WRITE(PUNIT,108) ERRCD
      ELSE IF (ERRCD.EQ.9) THEN
        WRITE(PUNIT,109) ERRCD
      ELSE IF (ERRCD.EQ.10) THEN
        WRITE(PUNIT,110) ERRCD
      ELSE IF (ERRCD.EQ.11) THEN
        WRITE(PUNIT,111) ERRCD
      ELSE IF (ERRCD.EQ.12) THEN
        WRITE(PUNIT,112) ERRCD
      ELSE IF (ERRCD.EQ.13) THEN
        WRITE(PUNIT,113) ERRCD
      ELSE IF (ERRCD.EQ.14) THEN
        WRITE(PUNIT,114) ERRCD
      ELSE IF (ERRCD.EQ.15) THEN
        WRITE(PUNIT,115) ERRCD
      ELSE IF (ERRCD.EQ.16) THEN
        WRITE(PUNIT,116) ERRCD
      ELSE IF(ERRCD.NE.0) THEN
        WRITE(PUNIT,120) ERRCD
      ENDIF
C
      RETURN
      END
C
C=====ERRPRNT Eof===========================================================
C
C=====HEADER bof===========================================================
C
C-----Purpose:
C     Programmed by: JM Fulford
C       Prints out header information,
C       and culvert and approach section info
C     Date:
C     Modified by:
C     Last modified:
C
C
      SUBROUTINE HEADER(PUNIT,PNO,HTITLE)
C
C-----Arguments:
      INTEGER PUNIT,PNO
      CHARACTER*80 HTITLE
C
C-----Module data:
      INCLUDE 'CULFLW.INC'
      INCLUDE 'CULVRT.INC'
      INCLUDE 'APPROC.INC'
C
C-----Local variables:
      REAL HEIGHT,MAPPEL
C
C-----Formats:
  201 FORMAT(A80,/,21X,'CULVERT',31X,'APPROACH SECTION',/,
     #       ' I.D. ',A16,'      Mannings n',5X,F5.3,8X,'I.D. ',
     #       A16)
  202 FORMAT(1X,'Height     ',F7.2,' ft',10X,' Width   ',F7.1,'ft',6X,
     #       'Station   ',F9.1,' ft'/,1X,'Station  ',F9.1,' ft',10X,
     #       'Length  ',F7.1,' ft',6X,'Minimum el. ',F8.2,'ft',/,
     #        1X,'Inlet el.  ',F8.2,'ft',7X,'Outlet el. ',F8.2,'ft')
  302 FORMAT(1X,'Height     ',F7.3,' m ',10X,' Width   ',F7.2,'m ',6X,
     #       'Station   ',F9.2,' m '/,1X,'Station  ',F9.2,' m ',10X,
     #       'Length  ',F7.2,' m ',6X,'Minimum el. ',F8.3,'m ',/,
     #        1X,'Inlet el.  ',F8.3,'m ',7X,'Outlet el. ',F8.3,'m ')
C
C-----write header info
      CALL VERSON(PUNIT,PNO)
      WRITE(PUNIT,201) HTITLE,VRTID,NRUFF,APPID
      MAPPEL=W1(1)+BASEL
      HEIGHT=BASEL+ZDROP
      IF(SI.EQ.0.OR.SI.EQ.3)THEN
        WRITE(PUNIT,202)D,CWIDTH,APSRD,CLSRD,CLEN,MAPPEL,HEIGHT,BASEL
      ELSE
        WRITE(PUNIT,302) ft_to_m(D),ft_to_m(CWIDTH)
     #   ,ft_to_m(APSRD),ft_to_m(CLSRD)
     #   ,ft_to_m(CLEN),ft_to_m(MAPPEL),ft_to_m(HEIGHT),ft_to_m(BASEL)
      ENDIF
C
      RETURN
      END
C
C=====HEADER eof===========================================================
C
C=====VERSON bof===========================================================
C
C-----Purpose:
C     Programmed by: JM Fulford
C       Prints out program version information
C     Date:
C     Modified by:
C     Last modified:
C
C
      SUBROUTINE VERSON (PUNIT,PNO)
C
C-----Arguments:
      INTEGER PNO,PUNIT
C
C-----local variables
      CHARACTER*6 VERS
C
      DATA VERS/'2021'/
C     version 97-08 a was compiled in 12-98 and is documented in the
C     wrir 98-4166 
C     version 97-08 b was compiled in 9-1999 minor fixes made    
C     version 2011c was compiled in December 2011 to run as batch or as interactive
C     version 2021 is a revision of 2011c (that was mislabled as 20011c). This
C                  version includes several major bug fixes (by Greg Koltun)
C
C-----Formats:
700   FORMAT (A1,'CAP - USGS culvert analysis program VER ',A6,25X,
     #        'page',I3,/)
701   FORMAT (/,'   CAP - USGS culvert analysis program VER ',A6,/)
C
      IF(PNO.GE.0)THEN
        WRITE(PUNIT,700)CHAR(12),VERS,PNO
      ELSE
        WRITE(*,701) VERS
      ENDIF
C
      RETURN
      END
C
C=====VERSON eof===========================================================


