C=====QTYP5 bof==============================================================
C-----Purpose:
C       Computes the discharge from the type 5 flow equation given
C       upstream approach water surface elevation.
C       Equation used is in TWRI Chapter A3, "Measurements of Peak
C       Discharge at Culverts by Indirect Methods",pg 30.
C     Programmed by: JM Fulford
C     Date:
C     Modified by:
C     Last modified:
C
      REAL FUNCTION QTYP5
     #                   (H1,ERR)
C
C-----Arguments:
      INTEGER ERR
      REAL H1
C
C-----Argument definitions:
C     H1     - upstream watersurface elevation
C     ERR    - error flag
C
C-----Module data:
      INCLUDE 'CULFLW.INC'
      INCLUDE 'CULVRT.INC'
C
C-----Local variables:
      INTEGER IERR
      REAL A0,K0,T0,C
C
C-----Externals:
C
C      H=(H1-ZDROP)/D
      C=C5INT(H1)
      D=CULD(TBSIZE)
      CALL CULINT(D,A0,K0,T0,IERR)
      ERR=IERR
      QTYP5=C*A0*SQRT(64.4*(H1-ZDROP))
C
      RETURN
      END
C
C=====QTYP5 eof==============================================================
C
C=====QTYP6 bof==============================================================
C-----Purpose:
C       Computes the discharge from the type 6 flow equation given
C       upstream approach water surface elevation.
C       Equation used is in TWRI Chapter A3, "Measurements of Peak
C       Discharge at Culverts by Indirect Methods",pg 30.
C     Programmed by: JM Fulford
C     Date:
C     Modified by:
C     Last modified:
C
      REAL FUNCTION QTYP6
     #                   (H1,ERR)
C
C-----Arguments:
      INTEGER ERR
      REAL H1
C
C-----Argument definitions:
C     H1     - upstream watersurface elevation
C     ERR    - error flag
C
C-----Module data:
      INCLUDE 'QTYP12.INC'
      INCLUDE 'CULVRT.INC'
C
C-----Local variables:
C
C-----Externals:
      REAL TYP6
      EXTERNAL TYP6, RTMIA
C
C-----Local variables:
      INTEGER ISTEP6
      REAL HR43,X,KF1,KF2,KF,QRAT,TOL6
C
      ISTEP6=400
      TOL6=0.0005
      ERR=0
      HR43=(CULK(TBSIZE)/(1.486*CULA(TBSIZE)))**2
      X=29.0*CLEN/HR43
      KF1=1.222081+X*(-0.39096055+X*(0.10127278-X*0.010547583))
      KF2=1.6475101+X*(-0.56519116+X*(0.12983789-X*0.011096691))
      CC=C46
      IF (CC.LE.0.76) THEN
        KF=KF1
      ELSE IF (CC.GE.1.0) THEN
        KF=KF2
      ELSE
        KF=KF1+4.166666*(CC-0.76)*(KF2-KF1)
      ENDIF
      D=CULD(TBSIZE)
C
      ERR=0
      HQ1=H1/D
      QRAT= HQ1*(4.7693-HQ1*(0.68351 - .044726*HQ1)) - 1.8368
      QRAT=HQ1*(7.2458-HQ1*(1.8350 -HQ1*(0.26087+0.014075*HQ1)))-3.6183
      QTYP6=QRAT*CULA(TBSIZE)*KF*SQRT(D)
C     because previous equations are from  x = f(y) and the orginal code
C     used a y = f(x) fit to fig 17 of the TWRI, the answer is not close
C     enough and a root solver is employed to get to the answer to ensure
C     consistency between solving for type 6 flow for Q and solving for
C     upstream head
      HA=0.75*QRAT
      HB=1.25*QRAT
C      CALL LAGROOT(HB,HA,TYP6,TOL6,ISTEP6,CONST,F,ERR)
      CALL RTMIA(CONST,F,TYP6,HA,HB,TOL6,ISTEP6,ERR)
      QTYP6=CONST*CULA(TBSIZE)*KF*SQRT(D)
C
      RETURN
      END
C
C=====QTYP6 eof==============================================================
C
C=====TYP6 bof==============================================================
C-----Purpose:
C       the type 6 flow equation given, used a similar form to solve for the
C       upstream approach water surface elevation.
C       Equation used is in TWRI Chapter A3, "Measurements of Peak
C       Discharge at Culverts by Indirect Methods",pg 30.
C     Programmed by: JM Fulford
C     Date:
C     Modified by:
C     Last modified:
C
      REAL FUNCTION TYP6
     #                   (CONST,ERR)
C
C-----Arguments:
      INTEGER ERR
      REAL CONST
C
C-----Argument definitions:
C     CONST  - Q/A*kf*SQRT(D)
C     ERR    - error flag
C
C-----Module data:
      INCLUDE 'QTYP12.INC'
C
C-----Local variables:
C
C-----Externals:
C
C-----Local variables:
C
C     Currently no error checking so set ERR=0      
      ERR=0
      TYP6=1.0993391-HQ1+CONST*(-0.1032313+CONST*
     #     (0.055182648-CONST*0.00098596166))
C
      RETURN
      END
C
C=====TYP6 eof==============================================================
C
C=====QTYP1 bof==============================================================
C-----Purpose:
C       Computes the discharge from the type 1 flow equation given
C       upstream approach water surface elevation.
C       Equation used is in TWRI Chapter A3, "Measurements of Peak
C       Discharge at Culverts by Indirect Methods",pg 30.
C     Programmed by: JM Fulford
C     Date:
C     Modified by:
C     Last modified:
C
      REAL FUNCTION QTYP1
     #                   (H1,ERR)
C
C-----Arguments:
      INTEGER ERR
      REAL H1
C
C-----Argument definitions:
C     H1     - upstream watersurface elevation
C     ERR    - error flag
C
C-----Module data:
      INCLUDE 'CULFLW.INC'
      INCLUDE 'QTYP12.INC'
C
C-----Local variables:
      INTEGER ISTEP1
      REAL A,K1,W,APH1,ADC,KDC,TOPW,HB,HA,TOL1
C
C-----Externals:
      REAL DCTYP1
      EXTERNAL KINTER, RTMIA, DCTYP1
C
      ISTEP1=400
      TOL1=.0005
      CALL KINTER(H1,A,K1,W,APH1,ERR)
      IF(ERR.NE.0) GO TO 999
      A1=A
      HQ1=H1
      VHD=APH1/(2.0*A1*A1)
      LK=32.2*LW/K1
      HB=D*0.98
      HA=D*0.1
C      CALL LAGROOT(HB,HA,DCTYP1,TOL1,ISTEP1,DC,F,ERR)
Cwrite      WRITE(*,*)'PRE RTMIA IN QTYP1'
      CALL RTMIA(DC,F,DCTYP1,HA,HB,TOL1,ISTEP1,ERR)
      IF(ERR.EQ.0)THEN
        CALL CULINT(DC,ADC,KDC,TOPW,ERR)
        QTYP1=SQRT(ADC*ADC*ADC*32.2/TOPW)
      ELSE
        QTYP1=0.0
      ENDIF
999   CONTINUE
C
      RETURN
      END
C
C=====QTYP1 eof==============================================================
C
C=====DCTYP1 bof==============================================================
C-----Purpose:
C       Computes the type 1 flow equation
C       given the the critical depth.
C       Equation used is in TWRI Chapter A3, "Measurements of Peak
C       Discharge at Culverts by Indirect Methods",pg 30.
C     Programmed by: JM Fulford
C     Date:
C     Modified by:
C     Last modified:
C
      REAL FUNCTION DCTYP1
     #                   (DC,ERR)
C
C-----Arguments:
      INTEGER ERR
      REAL DC
C
C-----Argument definitions:
C     H1     - upstream watersurface elevation
C     ERR    - error flag
C
C-----Module data:
      INCLUDE 'CULFLW.INC'
      INCLUDE 'QTYP12.INC'
C
C-----Local variables:
      INTEGER TYPFW
      REAL ADC,KDC,T0,A,CA,CC,ACT
C
C-----Externals:
C
      TYPFW=1
      A=A1
      CALL CULINT(DC,ADC,KDC,T0,ERR)
      CC=CADJST(HQ1,ADC,A,TYPFW)
      CA=1.0/CC
      ACT=ADC/T0
      DCTYP1 = HQ1-DC+ACT*ADC*ADC*(VHD-LK/KDC)-0.5*CA*CA*ACT-ZDROP
C
      RETURN
      END
C
C=====DCTYP1 eof==============================================================
C
C=====QTYP2 bof==============================================================
C-----Purpose:
C       Computes the discharge from the type 2 flow equations given
C       upstream approach water surface elevation.
C       Equation used is in TWRI Chapter A3, "Measurements of Peak
C       Discharge at Culverts by Indirect Methods",pg 30.
C     Programmed by: JM Fulford
C     Date:
C     Modified by:  JM Fulford
C     Last modified: 2.25.98 to correct error in equation formulation
C
      REAL FUNCTION QTYP2
     #                   (H1,ERR)
C
C-----Arguments:
      INTEGER ERR
      REAL H1
C
C-----Argument definitions:
C     H1     - upstream watersurface elevation
C     ERR    - error flag
C
C-----Module data:
      INCLUDE 'CULFLW.INC'
      INCLUDE 'QTYP12.INC'
C
C-----Local variables:
      INTEGER ISTEP2, TYPFW
      REAL A,K1,W,APH1,A2,K2,TOPW,KDC,ADC,HB,HA,F,DIFF,TOL2
      REAL CC, QNEW, QSQ, OLDDC
C
C-----Externals:
      REAL DCTYP2,H2TYP2
      EXTERNAL KINTER, RTMIA, DCTYP2, H2TYP2, CULINT, CADJST
C
      ISTEP2=400
      TOL2=0.0001
      CALL KINTER(H1,A,K1,W,APH1,ERR)
      IF(ERR.NE.0) GO TO 999
      A1=A
      VHD=APH1/(A1*A1)
      DC= D /2.0
10    CONTINUE
C
      CALL CULINT(DC,ADC,KDC,TOPW,ERR)
      QSQ=ADC*ADC*ADC*32.2/TOPW
      TYPFW =2
      CC = CADJST(H1,ADC,A,TYPFW)
      CC=1.0/CC
      CC = (CC*CC - 1.0)/(ADC*ADC)
      HQ1 = DC - ZDROP + QSQ/(64.4*ADC*ADC)
      ATW = QSQ
      LK = L*QSQ/KDC
      HB=D
      HA=DC
      CALL RTMIA(H2,F,H2TYP2,HA,HB,TOL2,ISTEP2,ERR)
      IF(ERR.EQ.0)THEN
        CALL CULINT(H2,A2,K2,TOPW,ERR)
        QSQ = (H1 - H2 - ZDROP)*64.4
        QSQ = QSQ/(1.0/(A2*A2) - VHD + 64.4*LW/(K1*K2) + CC)
        QNEW = SQRT(QSQ)
        OLDDC = DC
        HB=D-0.01
        HA=0.001*D
        LK=QSQ
        CALL RTMIA(DC,F,DCTYP2,HA,HB,TOL2,ISTEP2,ERR)
        IF(ERR.NE.0) THEN
          WRITE(*,*)'Error in root soln H2TYP2',ERR
        ELSE
          DIFF=DC-OLDDC
        ENDIF
      ELSE
C        WRITE(*,*)'ERROR IN QTYP2 - H2TYP2 ROOT SOLVE',ERR
C        looks like the else statement can be removed with its contents 
C        crouse corrected it by adding the following statement
        QTYP2=0.0
        GO TO 999
      ENDIF
      IF(ERR.EQ.0.AND.DIFF.GT.0.0001) GO TO 10
      IF(ERR.EQ.0)THEN
        QTYP2=QNEW
      ELSE
C        WRITE(*,*)'ERROR IN QTYP2 -  ROOT SOLVE'
        QTYP2=0.0
      ENDIF
999   CONTINUE
C
      RETURN
      END
C
C=====QTYP2 eof==============================================================
C
C=====H2TYP2 bof==============================================================
C-----Purpose:
C       compute equation for H2 given DC
C     Programmed by: JM Fulford
C     Date:
C     Modified by:
C     Last modified:
C
      REAL FUNCTION H2TYP2
     #                   (H2,ERR)
C
C-----Arguments:
      INTEGER ERR
      REAL H2
C
C-----Argument definitions:
C     H2     - inlet watersurface elevation
C     ERR    - error flag
C
C-----Module data:
      INCLUDE 'QTYP12.INC'
C
C-----Local variables:
      REAL AH2,KH2,T0
C
C-----Externals:
C
      CALL CULINT(H2,AH2,KH2,T0,ERR)
      H2TYP2=HQ1 - H2 + LK/KH2 - ATW/(64.4*AH2*AH2)
C
      RETURN
      END
C
C=====H2TYP2 eof==============================================================
C
C=====DCTYP2 bof==============================================================
C-----Purpose:
C       compute equation for DC given H2
C     Programmed by: JM Fulford
C     Date:
C     Modified by:
C     Last modified:
C
      REAL FUNCTION DCTYP2
     #                   (DC,ERR)
C
C-----Arguments:
      INTEGER ERR
      REAL DC
C
C-----Argument definitions:
C     H2     - inlet watersurface elevation
C     ERR    - error flag
C
C-----Module data:
      INCLUDE 'QTYP12.INC'
C
C-----Local variables:
      REAL ADC,KDC,TOPW
C
C-----Externals:
      CALL CULINT(DC,ADC,KDC,TOPW,ERR)
      DCTYP2=LK - ADC*ADC*ADC*32.2/TOPW
C
      RETURN
      END
C
C=====DCTYP2 eof==============================================================
C
C=====QTYP3 bof==============================================================
C-----Purpose:
C       compute equation for Q given H1
C     Programmed by: JM Fulford
C     Date:
C     Modified by:
C     Last modified:
C
      REAL FUNCTION QTYP3
     #                   (H1,TW,DC,ERR)
C
C-----Arguments:
      INTEGER ERR
      REAL H1,TW,DC
C
C-----Argument definitions:
C     H1     - approach section watersurface elevation
C     TW     - tailwater elevation
C     ERR    - error flag
C
C-----Module data:
      INCLUDE 'QTYP12.INC'
      INCLUDE 'CULFLW.INC'
C
C-----Local variables:
      INTEGER ISTEP2,TYPFW
      REAL TOL2,A,K1,W,APH1,AD3,K3,TOPW,CA,F,AD2,K2
C
C-----Externals
      REAL H2TYP3,CADJST
      EXTERNAL KINTER, RTMIA, H2TYP3, CULINT, CADJST
C
      ISTEP2=400
      TOL2=0.0005
      TYPFW=3
      CALL KINTER(H1,A,K1,W,APH1,ERR)
      IF(ERR.NE.0) GO TO 999
      CALL CULINT(TW,AD3,K3,TOPW,ERR)
Cwrite      WRITE(*,*)'PRE CADJST CALL QTYP3'
      CC=CADJST(H1,AD3,A,TYPFW)
Cwrite      WRITE(*,*)'H1,A,TW,AD3',H1,A,K1,TW,AD3,K3,CC
      CA=1/CC
c      VHD=((ZDROP-TW)*CA*CA-H1+TW)/(AD3*AD3)-(ZDROP-TW)*APH1/(A*A)
c      VHD=VHD/64.4
c      A1=(CA*CA/(AD3*AD3)-APH1/(A*A))/64.4
c      HQ1=(H1-TW)/64.4
c      LWLVH=L*(ZDROP-H1)/K3+LW*(ZDROP-TW)/K1
c      LK=(L/K3)+(LW/K1)
c      corrected? code 11-22-94
      HQ1=TW-ZDROP
      A1=1.0/(AD3*AD3)
      ATW=(H1-TW)/64.4
      LWLVH=(L/K3)+(LW/K1)
      LK=(H1-TW)*(L/K3)
      VHD=((CA*CA)/(AD3*AD3)-APH1/(A*A))/64.4
      HB=D
      HA=DC
      CALL RTMIA(H2,F,H2TYP3,HA,HB,TOL2,ISTEP2,ERR)
      IF(ERR.EQ.0)THEN
        CALL CULINT(H2,AD2,K2,TOPW,ERR)
        QTYP3=L/(K3*K2)+CA*CA/(64.4*AD3*AD3)+LW/(K2*K1)
     #         -APH1/(64.4*A*A)
        QTYP3=SQRT((H1-TW)/QTYP3)
      ELSE
Cwrite        WRITE(*,*)'Error in root soln H2TYP3',ERR
        QTYP3=0.0
      ENDIF
999   CONTINUE
C
      RETURN
      END
C
C=====QTYP3 eof==============================================================
C
C=====H2TYP3 bof==============================================================
C-----Purpose:
C       compute equation for type 3 as function of H2
C     Programmed by: JM Fulford
C     Date:
C     Modified by:
C     Last modified:
C
      REAL FUNCTION H2TYP3
     #                   (H2,ERR)
C
C-----Arguments:
      INTEGER ERR
      REAL H2
C
C-----Argument definitions:
C     H2     - inlet water surface depth
C     ERR    - error flag
C
C-----Module data:
      INCLUDE 'QTYP12.INC'
C
C-----Local variables:
      INTEGER LERR
      REAL AH2,KH2,TO
C
      LERR=0
      CALL CULINT(H2,AH2,KH2,TO,LERR)
      IF(LERR.NE.0)THEN
        H2TYP3=-1.0
        LERR=2
      ELSE
        IF(AH2.GT.0.AND.KH2.GT.0)
     #  H2TYP3=HQ1-H2+(ATW*(A1-1.0/(AH2*AH2))+(LK/KH2))/(VHD+LWLVH/KH2)
      ENDIF
      ERR=LERR
      RETURN
      END
C
C=====H2TYP3 eof=============================================================
C
C=====LINTERP bof==============================================================
C-----Purpose:
C       interpolate between type 1 or 2 and  5 or 6 the upstream water
C       elevation.
C     Programmed by: JM Fulford
C     Date:
C     Modified by:
C     Last modified:
C
      REAL FUNCTION LINTERP
     #                   (X1,X2,X,DEP,Z,F1,F2)
C
C-----Arguments:
      REAL X1,X2,X,DEP,Z,F1,F2
C
C-----Argument definitions:
C
C-----Module data:
C
C-----Local variables:
      F1=F1-F2
      LINTERP=F2*DEP+Z+F1*DEP*(X-X1)/(X2-X1)
C
      RETURN
      END
C
C=====LINTERP Eof==============================================================
