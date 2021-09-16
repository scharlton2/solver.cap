C=====CULRAT bof=============================================================
C
C-----Purpose:
C       computes flow through an approach section and a culvert section
C       this routine is the computational heart of the indirect measurement
C       in culverts program.
C     Programmed by: JM Fulford
C     Date:
C     Modified by:
C     Last modified:
C
      SUBROUTINE CULRAT
     I                 (XSCID,CLID)
C
C     Arguments:
      CHARACTER*16 XSCID,CLID
C
C     Argument definitions:
C     XSCID  - approach section id
C     CLID   - culvert structure id
C
C-----Module data:
      INCLUDE 'CULFLW.INC'
      INCLUDE 'FCNCN2.INC'
      INCLUDE 'CULVRT.INC'
C
C-----Local variables:
      REAL KO,KDC,K2,KD3,DCLO,DCHI,SO,SC,HA,HB
     #     ,CDEPTH,A2,AO,CA,CB,FA,FB,T0,H1E5FW,H1E6FW,
     #      PKON6,PKON5,QTEST,CQ2,F,F1,F2,
     #      D2A,Q3
      INTEGER ERR,J,IQC,IX,NH4,OLDTYP,TYP5,TYP6,ERRC,ERRCD5,ERRCD6
C
C-----Externals
      INTEGER IXMAX
      REAL TYP5FW,INLTD2,INLTD3,CULLEN,CULRD
     #     ,APPRD,QCRIT,CLDEPTH,C5INT,CHKD3,GETC46
      REAL QTYP1,QTYP2,QTYP5,QTYP6,QTYP3,LINTERP
      LOGICAL IXSPRP,ICVPRP
      EXTERNAL QCRIT,IXSPRP,ICVPRP,CULLEN,CULRD,APPRD,C5INT
      EXTERNAL TYPONE,TYPTWO,TYPTHRE,TYPFOUR,TYP5FW,INLTD2,INLTD3
      EXTERNAL QTYP1,QTYP2,QTYP5,QTYP6,QTYP3,LINTERP,CLDEPTH
      EXTERNAL TYP6FW,CHKD3,RTMIA,GETC46,IXMAX
C
C-----Initializations
      TOL = 0.0001
      ISTEPS=400
      ERR=0
      DCLO = 0.0
      D=CLDEPTH(CLID)
      DCHI = D - 0.005
      INUM = 0
      Q3=0.0
      Z = ZDROP
      L=CULLEN(CLID)
      LW=ABS(CULRD(CLID)-APPRD(XSCID))-L
      IF(LW.LE.0)THEN
Cwrite        WRITE(*,*)'L LW',L,LW
        ERR=-10
        CALL EROCUL (ERR,I)
      ELSE IF(LW.LT.CWIDTH) THEN
        ERR=-107
        CALL EROCUL (ERR,I)
        ERR=0
      ENDIF
      IF(ERR.NE.0) GO TO 100
C     compute endpoints for linear interpolation of discharges between
C     1.2D & 1.5D
      H1=1.2*D+Z
      Q1=QTYP1(H1,ERR)
      IF(ERR.NE.0)Q1=-1.0
      H1=1.25*D+Z
      Q2=QTYP2(H1,ERR)
      IF(ERR.NE.0)Q2=-1.0
      H1=1.5*D+Z
      Q5=QTYP5(H1,ERR)
      IF(ERR.NE.0)Q5=-1.0
      H1=1.75*D+Z
      Q6=QTYP6(H1,ERR)
      IF(ERR.NE.0)Q6=-1.0
C
C
C     + + zero common block array stuff(except for q & tw) + +
      ERR=0
      NH4=NQ*NTW*2
      DO 70 IX=1,NH4
        H1E(IX)=-1.0
        D2(IX)=-1.0
        D3A(IX)=-1.0
        TYPE(IX)=0
        PKON(IX)=0.0
        ERRCOD(IX)=0
 70   CONTINUE
C
C
      SO=Z/L
      DO 99 IQC=1,NQ
C     + + Compute critical depth in culvert for discharge CQ  + +
        CQ=Q(IQC)
        CQ2=Q(IQC)*Q(IQC)
        QG=CQ2/64.4
        DC=0.0
        DCLO=0.0
        CALL RTMIA(DC,QTEST,QCRIT,DCLO,DCHI,TOL,ISTEPS,ERR)
        IF(ERR.EQ.-2.OR.ERR.EQ.-3)THEN
C     + + critical depth is larger that culvert height
C         if dc > d messes up how type 3 processes high head stuff
          DC=D*1.05
          SC=SO*1.1
          IF(SC.LT.0) SC=0.
        ELSE IF(ERR.EQ.0)THEN
C     + + check for culvert slope
          DCLO = DC
          CALL CULINT(DC,ADC,KDC,T0,ERR)
          VGDC=QG/(ADC*ADC)
          H12DC=LW*CQ2/KDC
          H23DC=L*CQ2/KDC
          DCPZ=DC+Z
          DCMZ=DC-Z
          SC=(CQ/KDC)**2
          IF(SC.LT.0) SC=0.
          IF(SC.LE.SO) THEN
C           culvert slope is steep
            CDEPTH=DCPZ
          ELSE
C           culvert slope is mild
            CDEPTH=DC
          ENDIF
          ELSE
            WRITE(*,*)'ERROR solving for critical depth',ERR
        ENDIF
C
        DO 98 J=1,NTW
Cread       READ(*,*)IXXK
          ERR=0
          INUM=INUM+1
          H1E(INUM)=0.0
          H4E(INUM)=H4TW(J)+BASEL
          CNTRS(INUM)=IQC
          H4TWID(INUM)=J
          CULCRTD(INUM)=DC
          ERR=0
          D3=H4TW(J)
          F=H4EST()+D3
Cwrite          IF(D3.LE.0)THEN
Cwrite          WRITE(*,*)H4TW
Cwrite          READ(*,*)IXXK
Cwrite          ENDIF
          IF(SC.LE.SO.AND.H4TW(J).LE.(DC+Z).AND.DC.LE.D) THEN
C           + + Type one culvert flow - critical depth at culvert inlet  + +
              CALL TYPONE(ERR)
C            ELSE
          ELSE IF (SC.GT.SO.AND.H4TW(J).LE.DC.AND.DC.LE.D) THEN
C           + + Type two culvert flow - critical depth at culvert outlet + +
              CALL TYPTWO(ERR)
C            ENDIF
          ELSE
            IF(H4TW(J).GT.D) THEN
C           + + Type four culvert flow - submerged outlet & inlet        + +
              TYPE(INUM)=4
              D2(INUM)=D
              D2A=D2(INUM)
              CALL CULINT(D2A,A2,K2,TO,ERR)
              CC=GETC46()
              H23D3=L*CQ2/(K2*K2)+QG/(A2*A2*CC*CC) + H4TW(J)
              H12D2=LW*CQ2/K2
              TW=H4TW(J)
              CALL TYPFOUR(TW,ERR)
            ELSE IF(DC.LT.D) THEN
C             + + Type three culvert flow - tranquil flow throughout
              TYPE(INUM)=3
              D3A(INUM)=H4TW(J)
              D3=D3A(INUM)
              TW=H4TW(J)
              H1=1.25*D+Z
              Q3=QTYP3(H1,TW,DC,ERR)
              IF(ERR.NE.0)Q3=-1.0
              CALL CULINT (TW,AD3,KD3,T0,ERR)
              IF(ERR.EQ.0)THEN
                H23D3=L*CQ2/KD3
                VGD3=QG/(AD3*AD3)
Cwrite                WRITE(*,*)'PRE TYPTHRE CALL'
                CALL TYPTHRE(TW,ERR)
Cread           READ(*,*)IXXK
              ENDIF
            ELSE
              TYPE(INUM)=0
              ERR=-2
            ENDIF
          ENDIF
C
          IHIGH=0
          IF(TYPE(INUM).EQ.1) THEN
            IF(H1E(INUM).GT.(1.2*D+Z+BASEL).AND.H4TW(J).LE.D) IHIGH=1
          ELSE IF(TYPE(INUM).NE.1.AND.TYPE(INUM).NE.4)THEN
            IF(H1E(INUM).GT.(1.25*D+Z+BASEL).AND.H4TW(J).LE.D)IHIGH=1
          ENDIF
          IF(ERR.LE.-2.AND.H4TW(J).LE.D.OR.IHIGH.EQ.1) THEN
C     #      (H1E(INUM).GT.(1.2*D+Z+BASEL).AND.H4TW(J).LT.D)) THEN
C         + + traps cases where didn't bracket on: type 3 flow eq'n,
C         + + type 2 inlet eq'n, type 2 flow eq'n, type 1 flow eq'n
C         + + and cases where DC>D and tailwater<D.  All possible
C         + + high head flow situations.
C            ERR=0
            OLDTYP=TYPE(INUM)
            TYP5=5
C           + + Type six culvert flow - full flow with free outfall
            H1E6FW=TYP6FW()
            ERRCD6=0
            IF(H1E6FW.GT.6.5) THEN
C             warning exceeds graphed values
              ERRCD6=15
            ENDIF
            H1E6FW=D*H1E6FW
            IF(H1E6FW.LT.0) THEN
              ERRCD6=16
              H1E6FW=-1.0
            ENDIF
            PKON6=CC
            TYP6=6
C         + + Type five culvert flow - rapid flow at inlet            + +
            CALL CULINT(D,AO,KO,T0,ERRC)
            IF(ERRC.EQ.0) THEN
              VGD3=QG/(AO*AO)
              HA=1.2*D+Z
              HB=5.0*D+Z
              CA=C5INT (HA)
              PKON5=CA
              CB=C5INT (HB)
              FA=VGD3/(CA*CA)
            IF(FA.GE.(1.2*D-Z).OR.ERR.EQ.0) THEN
              FB=VGD3/(CB*CB)
              IF(FB.GT.5.*D)THEN
                H1E5FW=FB+Z
                ERR=0
              ELSE
                 H1E5FW=0.0
                CALL RTMIA (H1E5FW,F,TYP5FW,HA,HB,TOL,ISTEPS,ERR)
              ENDIF
              IF(ERR.EQ.0) THEN
                ERRCD5=0
                PKON5=CC
              ELSE IF(ERR.EQ.-2.OR.ERR.EQ.-3) THEN
                ERRCD5=7
                IF(ERR.EQ.-3) THEN
                  ERR=0
                  H1E5FW=-1.0-BASEL
                ELSE
                  H1E5FW=-1.0-BASEL
                  ERR=-5
                  ERRCD5=ERR
                ENDIF
              ELSE
                ERRCD5=ERR
                ERR=0
              ENDIF
            ELSE IF(ERR.NE.0) THEN
C           + + not a high head flow , make do calculation, not really
C           + + correct but should be close
Cwrite              WRITE(*,*)'TYPE ERR',OLDTYP,ERR,BASEL
Cwrite              WRITE(*,*)'PROBABLE SPATIAL CONVERGENCE ERROR'
c              READ(*,*)IXXK
              TYPE(INUM)=90+TYPE(INUM)
              PKON(INUM)=CC
              H1E(INUM)=-1
              D2(INUM)=-1
              D3A(INUM)=-1
              ERRCOD(INUM)=ERR
            ENDIF
C
            IF(TYPE(INUM).LT.90)THEN
            IF(H1E5FW.LT.(1.5*D+Z).AND.ERR.NE.-5)THEN
C           + + interpolate between flow type 5 and type 1 or type 2
              ERRCD5=11
              F1=1.50
              IF(OLDTYP.EQ.1)THEN
                IF(Q1.GT.0)THEN
                F2=1.2
                H1E5FW=LINTERP(Q1,Q5,CQ,D,Z,F1,F2)
                TYP5=15
                ELSE IF(Q2.GT.0)THEN
                F2=1.25
                H1E5FW=LINTERP(Q2,Q5,CQ,D,Z,F1,F2)
                TYP5=25
                ENDIF
              ELSE IF(OLDTYP.EQ.2.OR.OLDTYP.EQ.42)THEN
                IF(Q2.GT.0)THEN
                F2=1.25
                H1E5FW=LINTERP(Q2,Q5,CQ,D,Z,F1,F2)
                TYP5=25
                ELSE IF(Q1.GT.0)THEN
                F2=1.2
                H1E5FW=LINTERP(Q1,Q5,CQ,D,Z,F1,F2)
                TYP5=15
                ENDIF
              ELSE IF(OLDTYP.EQ.3.OR.OLDTYP.EQ.43)THEN
                F2=1.25
                IF(Q3.LE.0)THEN
                  ERRCD5=10
                  H1E5FW=LINTERP(Q2,Q5,CQ,D,Z,F1,F2)
                  TYP5=52
                ELSE
                  H1E5FW=LINTERP(Q3,Q5,CQ,D,Z,F1,F2)
                  TYP5=35
                ENDIF
              ELSE IF(OLDTYP.EQ.0)THEN
                F2=1.20
                H1E5FW=LINTERP(Q1,Q5,CQ,D,Z,F1,F2)
                TYP5=50
              ELSE
Cwrite                WRITE(*,*)'In high head interp oldtyp',OLDTYP
Cwrite                READ(*,*)IXXK
              ENDIF
            ENDIF
            IF(H1E6FW.LT.(1.75*D+Z).AND.H1E6FW.GT.0.0 )THEN
C           + + interpolate between flow type 6 and type 1, 2 or 3
              ERRCD6=11
              F1=1.75
              IF(OLDTYP.EQ.1)THEN
                IF(Q1.GT.0)THEN
                F2=1.20
                H1E6FW=LINTERP(Q1,Q6,CQ,D,Z,F1,F2)
                TYP6=16
                ELSE IF(Q2.GT.0)THEN
                F2=1.25
                H1E6FW=LINTERP(Q2,Q6,CQ,D,Z,F1,F2)
                TYP6=26
                ENDIF
              ELSE IF(OLDTYP.EQ.2.OR.OLDTYP.EQ.42)THEN
                IF(Q2.GT.0)THEN
                F2=1.25
                H1E6FW=LINTERP(Q2,Q6,CQ,D,Z,F1,F2)
                TYP6=26
                ELSE IF(Q1.GT.0)THEN
                F2=1.2
                H1E6FW=LINTERP(Q1,Q6,CQ,D,Z,F1,F2)
                TYP6=16
                ENDIF
              ELSE IF(OLDTYP.EQ.3.OR.OLDTYP.EQ.43)THEN
                F2=1.25
                IF(Q3.LE.0)THEN
                  ERRCD6=10
                  H1E6FW=LINTERP(Q2,Q6,CQ,D,Z,F1,F2)
                  TYP6=62
                ELSE
                  TYP6=36
                  H1E6FW=LINTERP(Q3,Q6,CQ,D,Z,F1,F2)
                ENDIF
              ELSE IF(OLDTYP.EQ.0)THEN
                F2=1.2
                H1E6FW=LINTERP(Q1,Q6,CQ,D,Z,F1,F2)
                TYP6=60
              ELSE
Cwrite               WRITE(*,*)'In high head interp oldtyp',OLDTYP
Cread           READ(*,*)IXXK
              ENDIF
            ENDIF
C
C
            IF(TFLW.EQ.5.OR.TFLW.EQ.65)THEN
C             put out five type flow
              IF(ERR.EQ.0)THEN
                H1E(INUM)=H1E5FW + BASEL
                D2(INUM)=D
                IF(TYP5.LE.5)THEN
                  D3A(INUM)=DC
                ELSE
                  D3A(INUM)=-1.00
                ENDIF
                PKON(INUM)=PKON5
                TYPE(INUM)=TYP5
                ERRCOD(INUM)=ERRCD5
              ENDIF
            ENDIF
            IF(TFLW.EQ.65) THEN
C             step inum counter to include extra output
              INUM=INUM+1
C           gfk - added line below to prevent having a zero array index stored in H4TWID
              H4TWID(INUM)=J
              CNTRS(INUM)=CNTRS(INUM-1)
              CULCRTD(INUM)=CULCRTD(INUM-1)
              H4E(INUM)=H4E(INUM-1)
            ENDIF
            IF(TFLW.EQ.6.OR.TFLW.EQ.65.OR.HFLW.LT.H1E(INUM))THEN
C             put type six flow
              IF(H1E6FW.LT.0.0) THEN
                ERRCOD(INUM)=16
                PKON(INUM)=PKON6
                TYPE(INUM)=TYP6
                D2(INUM)=-1
                D3A(INUM)=-1
                H1E(INUM)=-1 
              ELSE IF (ERR.EQ.0.OR.ERR.EQ.-5)THEN
                H1E(INUM)=H1E6FW + BASEL
                IF(TYP6.LE.6)THEN
                  D3A(INUM)=D
                ELSE
                  D3A(INUM)=-1.00
                ENDIF
                PKON(INUM)=PKON6
                TYPE(INUM)=TYP6
                D2(INUM)=D
                ERRCOD(INUM)=ERRCD6
              ENDIF
            ENDIF
            ENDIF
            ENDIF
          ENDIF
          IF(ERR.GE.-1.AND.ERR.NE.0) ERRCOD(INUM)=ERR
Cwrite          WRITE(*,*)'MAIN LOOP COUNT J,IQC,INUM',J,IQC,INUM
 98     CONTINUE
 99   CONTINUE
C
100   CONTINUE
      RETURN
      END
C
C=====CULRAT eof=============================================================
C
C=====TYPONE bof==============================================================
C
C-----Purpose:
C       Solves the energy equation for type one culvert flow.
C       Equation used is in TWRI Chapter A3, "Measurements of Peak
C       Discharge at Culverts by Indirect Methods",pg 2.
C     Programmed by: JM Fulford
C     Date:
C     Modified by:
C     Last modified:
C
      SUBROUTINE TYPONE (ERR)
C
C-----Dummy arguments:
      INTEGER ERR
C
C-----Module data:
      INCLUDE 'FCNCN2.INC'
      INCLUDE 'CULFLW.INC'
C
C-----Local variables:
      INTEGER KERR
      REAL H1,HA,HB,F,AA,K,CAB,AAPH,K2,TO,A2,D2A,TW
C
C-----Externals:
      REAL H1EST,H1CMP,TYP1FW,LINTERP
      EXTERNAL H1EST,TYP1FW,KINTER,LINTERP,RTMIA
C
      TYPE(INUM)=1
C     note that h1 is reference to the datum in this case
      H1=H1CMP(Z,ERR)
      IF(H1.GT.0.AND.ERR.EQ.0)THEN
        HA=H1
        HB=1.5*D+Z
111     CONTINUE
        CALL RTMIA(H1E(INUM),F,TYP1FW,HA,HB,TOL,ISTEPS,ERR)
        IF(ERR.EQ.0)THEN
            D2(INUM)=DC
            ERRCOD(INUM)=0
            H1E(INUM)=H1E(INUM)+BASEL
            D3A(INUM)=-1
            PKON(INUM)=CC
            CALL KINTER (H1E(INUM),AA,K,CAB,AAPH,KERR)
C          ENDIF
        ELSE IF(ERR.EQ.-2.OR.ERR.EQ.-3)THEN
          CALL KINTER (H1,AA,K,CAB,AAPH,KERR)
          H1E(INUM)=-1
          D2(INUM)=-1
          D3A(INUM)=-1
          IF (CQ.LE.AA*SQRT(32.2*AA/CAB).AND.ERR.NE.-2) THEN
C         + + supercritical flow in approach section
            ERRCOD(INUM)=4
Cread         READ(*,*)IXXK
            ERR=4
          ELSE IF(ERR.NE.-2)THEN
C           didn't close near the lower limit, no possibility of
C           high head flow, either super critical in approach section
C           or a spatial convergence problem has occurred.
            ERR=-4
            ERRCOD(INUM)=ERR
          ELSE IF(ERR.EQ.-2.AND.H4EST()+D3.GE.D+Z.AND.D3.GT.D)THEN
Cwrite            WRITE(*,*)'H4EST',H4EST()
C           + + Type four culvert flow - submerged outlet & inlet        + +
              TYPE(INUM)=4
              D2(INUM)=D
              D2A=D2(INUM)
              CALL CULINT(D2A,A2,K2,TO,ERR)
              CC=GETC46()
Cwrite              WRITE(*,*)'TYPEONE 4 -A2,C46,K2',A2,CC,K2,D2A
              TW=D3
              H23D3=L*CQ*CQ/(K2*K2)+QG/(A2*A2*CC*CC) + TW
              H12D2=LW*CQ*CQ/K2
              CALL TYPFOUR(TW,ERR)
          ENDIF
        ELSE
Cread     WRITE(*,*)'TYPEONE ERR=',ERR,CQ,H1,H1E(INUM),INUM
Cread     READ(*,*)IXXK
          H1E(INUM)=-1
          D2(INUM)=-1
          D3A(INUM)=-1
        ENDIF
      ELSE IF (ERR.EQ.1) THEN
        ERRCOD(INUM)=ERR
        H1E(INUM)=-1
        D2(INUM)=-1
        D3A(INUM)=-1
      ELSE
        ERRCOD(INUM)=6
        ERR=6
      ENDIF
Cwrite      WRITE(*,*)'ERRCOD, ERR, TYPONE',ERRCOD(INUM),ERR,D3,D
Cwrite      READ(*,*)IXXXK
C
      RETURN
      END
C
C=====TYPONE eof==============================================================
C
C=====TYP1FW bof==============================================================
C
C-----Purpose:
C       Function computes the energy equation for type one culvert flow.
C       Equation used is in TWRI Chapter A3, "Measurements of Peak
C       Discharge at Culverts by Indirect Methods",pg 2.
C     Programmed by: JM Fulford
C     Date:
C     Modified by:
C     Last modified:
C
      REAL FUNCTION TYP1FW
     I                (H1,ERR)
C
C-----Arguments:
      INTEGER ERR
      REAL H1
C
C-----Module data:
      INCLUDE 'FCNCN2.INC'
      INCLUDE 'CULFLW.INC'
C
C-----Local variables:
      INTEGER TYPFW
      REAL CA,A1,K1,APH1,W
C
C-----Externals
      REAL CADJST
      EXTERNAL KINTER,CADJST
C
      TYPFW=1
      CALL KINTER(H1,A1,K1,W,APH1,ERR)
      CC=CADJST(H1,ADC,A1,TYPFW)
      IF(CC.EQ.0) ERR=3
C
      IF(ERR.EQ.0.AND.K1.GT.0.0)THEN
      CA=1.0/CC
      TYP1FW = DCPZ +H12DC/K1 +(VGDC*CA*CA) -H1 -(APH1*QG/(A1*A1))
      ELSE
        ERR=1
      ENDIF
C
      RETURN
      END
C
C=====TYP1FW bof==============================================================
C
C=====TYPTWO eof==============================================================
C
C-----Purpose:
C       Solves the energy equation for type two culvert flow.
C       Equation used is in TWRI Chapter A3, "Measurements of Peak
C       Discharge at Culverts by Indirect Methods",pg 2.
C     Programmed by: JM Fulford
C     Date:
C     Modified by:
C     Last modified:
C
      SUBROUTINE TYPTWO (ERR)
C
C-----Dummy arguments:
      INTEGER ERR
C
C-----Module data:
      INCLUDE 'FCNCN2.INC'
      INCLUDE 'CULFLW.INC'
C
C-----Local variables:
      INTEGER IERRR
      REAL HB,HA,A2,K2,T0,DA,DB,K,CAB,AAPH,LPART,TW,D2A,F
C
C-----Externals
      REAL INLTD2,TYP2FW,LINTERP,TYP4LEN,GETC46
      EXTERNAL RTMIA,CULINT,LINTERP,INLTD2,TYP2FW,TYPFOUR,TYP4LEN,
     #         GETC46
C
      TYPE(INUM)=2
      DA=DC
Cwrite      WRITE(*,*)'TYPTWO DA=',DA
      F=INLTD2(DA,ERR)
Cwrite      WRITE(*,*)'DA, F',DA,F,ERR
      DB=D
      F=INLTD2(DB,ERR)
Cwrite      WRITE(*,*)'DB, F',DB,F,ERR
      CALL RTMIA (D2(INUM),F,INLTD2,DA,DB,TOL,ISTEPS,ERR)
      IF(ERR.EQ.-3)THEN
C     supercritical flow in the approach
        D2(INUM)=D
        CALL CULINT(D2(INUM),A2,K2,T0,IERRR)
        ERRCOD(INUM)=4
      ELSE IF(ERR.EQ.-2)THEN
C     inlet end is flowing full, compute using type four for length
C     of culvert that is flowing full
        LPART=TYP4LEN(D,DC)
        TW=D+(L-LPART)*Z/L
        IF(Z.LT.0) TW=D
        CC=GETC46()
        D2(INUM)=D
        D2A=D2(INUM)
        CALL CULINT(D2A,A2,K2,TO,ERR)
        IF(ERR.EQ.0)THEN
          H23D3=LPART*CQ*CQ/(K2*K2) + QG/(A2*A2*CC*CC) +TW
          H12D2=LW*CQ*CQ/K2
          CALL TYPFOUR(TW,ERR)
          IF(ERR.NE.0) THEN
Crea      WRITE(*,*)'*******TYPFOUR 42 ERR, TW',ERR,TW
Cread     READ(*,*)IXXXK
          ENDIF
          TYPE(INUM)=42
          ERRCOD(INUM)=13
          D3A(INUM)=DC
        ENDIF
Cread   READ(*,*)IXXK
      ELSE IF(ERR.EQ.0)THEN
        CALL CULINT(D2(INUM),A2,K2,T0,ERR)
        IF(ERR.EQ.0)THEN
          H23DCK=H23DC/K2
          H12D2=LW*CQ*CQ/K2
          HA=D2(INUM)+Z
          HB=1.5*D+Z
          F=TYP2FW(HA,ERR)
Cwrite          WRITE(*,*)'HA,ERR,F',HA,ERR,F
          F=TYP2FW(HB,ERR)
Cwrite          WRITE(*,*)'HB,ERR,F',HB,ERR,F
Cwrite          READ(*,*)IXXXK
          CALL RTMIA (H1E(INUM),F,TYP2FW,HA,HB,TOL,ISTEPS,ERR)
        ENDIF
        IF(ERR.EQ.0)THEN
          D3A(INUM)=DC
          ERRCOD(INUM)=0
          H1E(INUM)=H1E(INUM)+BASEL
          PKON(INUM)=CC
        ELSE IF (ERR.EQ.-2.OR.ERR.EQ.-3) THEN
C       + + root not bracketed in type 2 flow eq'n
Cwrite          WRITE(*,*)'AFTER TYP2FW CALL',ERR
          HA=D2(INUM)+Z
          CALL KINTER (HA,AA,K,CAB,AAPH,KERR)
          IF (CQ.GT.AA*SQRT(32.2*AA/CAB)) THEN
C         + + supercritical flow in approach section
            ERR=5
          ELSE IF(ERR.EQ.-3) THEN
C           didn't close near the lower limit, no possibility of
C           high head flow, either super critical in approach section
C           or a spatial convergence problem has occurred.
            ERR=-4
          ENDIF
          H1E(INUM)=-1
          D2(INUM)=-1
          D3A(INUM)=-1
          ERRCOD(INUM)=ERR
        ELSE
          H1E(INUM)=-1
          D2(INUM)=-1
          D3A(INUM)=-1
          ERRCOD(INUM)=ERR
        ENDIF
      ENDIF
C
      RETURN
      END
C
C=====TYPTWO Eof==============================================================
C
C=====INLTD2 bof==============================================================
C
C-----Purpose:
C       Computes the energy equation from the culvert entrance to the
C       culvert exit to determine the depth of flow at the
C       culvert entrance for type 2 flow.
C       Equation used is in TWRI Chapter A3, "Measurements of Peak
C       Discharge at Culverts by Indirect Methods",pg 25.
C     Programmed by: JM Fulford
C     Date:
C     Modified by:
C     Last modified:
C
      REAL FUNCTION INLTD2
     I                (H2,ERR)
C
C-----Arguments:
      REAL H2
C
C-----Argument definitions:
C     H2     - water surface elevation at culvert entrance
C     ERR    - error flag
C
C-----Module data:
      INCLUDE 'FCNCN2.INC'
C
C-----Local variables:
      INTEGER ERR
      REAL A2,K2,T0
C
C-----Externals:
      EXTERNAL CULINT
C
      CALL CULINT(H2,A2,K2,T0,ERR)
      IF(ERR.EQ.0)THEN
      IF (A2.EQ.0.OR.K2.EQ.0) WRITE(*,*)'A2 OR K2 =0',H2
      INLTD2 = DCMZ +VGDC -H2 -(QG/(A2*A2)) +H23DC/K2
      ELSE
        WRITE(*,*)'CULINT ERROR',H2,ERR
      ENDIF
C
      RETURN
      END
C
C=====INLTD2 eof==============================================================
C
C=====TYP2FW bof==============================================================
C
C-----Purpose:
C       Computes the energy equation for type two culvert flow.
C       Equation used is in TWRI Chapter A3, "Measurements of Peak
C       Discharge at Culverts by Indirect Methods",pg 2.
C     Programmed by: JM Fulford
C     Date:
C     Modified by:
C     Last modified:
C
      REAL FUNCTION TYP2FW
     I                (H1,ERR)
C
C-----Arguments:
      REAL H1
C
C-----Argument definitions:
C
C-----Module data:
      INCLUDE 'FCNCN2.INC'
C
C-----Local variables:
      INTEGER ERR,TYPFW
      REAL CA,A1,K1,APH1,W
C
C-----Externals:
      REAL CADJST
      EXTERNAL CADJST, KINTER
C
      TYPFW=2
      CALL KINTER(H1,A1,K1,W,APH1,ERR)
      IF(ERR.EQ.0)THEN
        CC=CADJST(H1,ADC,A1,TYPFW)
        IF(CC.EQ.0) ERR=3
        IF(ERR.EQ.0)THEN
          CA=1.0/CC
          TYP2FW = DC+H23DCK-H1 -APH1*QG/(A1*A1) +VGDC*CA*CA +H12D2/K1
        ENDIF
      ENDIF
C
      RETURN
      END
C
C=====TYP2FW eof==============================================================
C
C=====TYPTHRE bof=============================================================
C-----Purpose:
C       Solves the energy equation for type 3 culvert flow.
C     Programmed by: JM Fulford
C     Date:
C     Modified by:
C     Last modified:
C
      SUBROUTINE TYPTHRE
     I                  (TW,ERR)
C
C-----Arguments:
      INTEGER ERR
      REAL TW
C
C-----Module data:
      INCLUDE 'FCNCN2.INC'
      INCLUDE 'CULFLW.INC'
      INCLUDE 'APPROC.INC'
C
C-----Local variables:
      REAL HA,HB,LPART,AC,KC,TO,A2,K2,D2A,F,K3,AA,AK
C
C-----Externals:
      REAL INLTD3,TYP4LEN,TYP3FW,GETC46,NORDEP
      EXTERNAL RTMIA,INLTD3,TYPONE,TYPTWO,TYP4LEN,CULINT,TYPFOUR,
     #         TYP3FW,GETC46,NORDEP,TYP3SW
C
      ERR=0
      HA=DC
      HB=D
      D3=TW
      F=INLTD3(HA,ERR)
      F=INLTD3(HB,ERR)
      IF(HA.GE.HB) ERR=-3
      IF(ERR.EQ.-3) GOTO 999
      CALL RTMIA(D2A,F,INLTD3,HA,HB,TOL,ISTEPS,ERR)
      IF(ERR.EQ.-3) THEN
C       Lower bracket nearest root
        CALL CULINT(DC,AC,KC,TO,ERR)
          F=L*(CQ/KC)**2
Cwrite          WRITE(*,*)'TYPE 3 CALL TYPONE ',Z,F
Cwrite          READ(*,*)IXXXK
        IF(Z.GE.L*((CQ/KC)**2)) THEN
C       culvert slope is steep, critical depth must occur at inlet
C       two flow regimes present in culvert, rapid flow and tranquil
C       at the exit section.  Hydraulic jump occurs in culvert.
          ERR=0
          CALL TYPONE(ERR)
          D3A(INUM)=-1
          ERRCOD(INUM)=8
        ELSE
C       culvert slope is mild, normal depth of flow occurs at inlet
          HA=DC
          HB=D3
          CALL RTMIA(D2A,F,NORDEP,HA,HB,TOL,ISTEPS,ERR)
          IF(ERR.NE.0) THEN
            WRITE(*,*)'NORMAL DEP type3-high head ERR',ERR,F,Z
     +      ,CQ,TW
          ELSE
            CALL CULINT(D2A,A2,K2,TO,ERR)
            LPART=(D3+VGD3-D2A-QG/(A2*A2))/(H23D3/K2-(Z/L))
            CALL CULINT(D3,AC,K3,TO,ERR)
            H23D3=(CQ*CQ)*(LPART/(K2*K3) + (L-LPART)/(K2*K2))
          ENDIF
Cread     READ(*,*)IXXXK
          D2(INUM)=D2A
          IF(ERR.EQ.0.AND.L.GE.LPART) THEN
            VGD2=QG/(A2*A2)
            H12D2=LW*CQ*CQ/K2
            HA=D2(INUM)+Z
            HB=1.5*D+Z
            CALL RTMIA(H1E(INUM),F,TYP3FW,HA,HB,TOL,ISTEPS,ERR)
          ENDIF
          IF(ERR.NE.0.OR.L.LT.LPART)THEN
            ERRCOD(INUM)=9
            H1E(INUM)=-1
            D2(INUM)=-1
            D3A(INUM)=-1
          ELSE
            PKON(INUM)=CC
            H1E(INUM)=H1E(INUM)+BASEL
            H4E(INUM)=TW+BASEL
            D3A(INUM)=D3
            ERRCOD(INUM)=9
          ENDIF
        ENDIF
      ELSE IF(ERR.EQ.-2) THEN
C       Upper bracket nearest root, inlet end of culvert is full and
C       outlet end of culvert is part full
        IF(TYPE(INUM).EQ.34) WRITE(*,*)'WARNING--possible error',
     #  ' computing from type 34 to 43'
        TYPE(INUM)=43
Cwrite        WRITE(*,*)'***************************IN 43 CALS INUM=',INUM
        D3A(INUM)=TW
Cwrite        WRITE(*,*)'D,TW',D,TW
        LPART=TYP4LEN(D,TW)
Cwrite        WRITE(*,*)'LPART, L, Z',LPART,L,Z
        TW=D+(L-LPART)*Z/L
        IF(Z.LT.0) TW=D
        CC=GETC46()
Cwrite        WRITE(*,*)'CC',CC
        D2(INUM)=D
        D2A=D2(INUM)
        CALL CULINT(D2A,A2,K2,TO,ERR)
Cwrite        WRITE(*,*)'43 CAL CULINT CALL',ERR
        IF(ERR.EQ.0)THEN
          H23D3=LPART*CQ*CQ/(K2*K2) + QG/(A2*A2*CC*CC) + TW
          H12D2=LW*CQ*CQ/K2
          CALL TYPFOUR(TW,ERR)
          ERRCOD(INUM)=13
          D3A(INUM)=D3
Cwrite          WRITE(*,*)'INUM AFTER TYPFOUR 43 CALL',INUM
Cwrite          READ(*,*) IKKK
        ENDIF
      ELSE IF(ERR.EQ.0) THEN
        D2(INUM)=D2A
        CALL CULINT(D2A,A2,K2,TO,ERR)
        IF(ERR.EQ.0)THEN
          VGD2=QG/(A2*A2)
          H12D2=LW*CQ*CQ/K2
          H23D3=H23D3/K2
          HA=D2(INUM)+Z
          HB=1.5*D+Z
          CALL RTMIA (H1E(INUM),F,TYP3FW,HA,HB,TOL,ISTEPS,ERR)
        ENDIF
        IF (ERR.EQ.0)THEN
          PKON(INUM)=CC
          H1E(INUM)=H1E(INUM)+BASEL
          H4E(INUM)=TW+BASEL
          D3A(INUM)=D3
          ERRCOD(INUM)=0
        ELSE
          HA=D2(INUM)+Z
          CALL KINTER (HA,AA,AK,CAB,AAPH,KERR)
          IF (CQ.GT.AA*SQRT(32.2*AA/CAB)) THEN
C         + + supercritical flow in approach section
            ERR=5
          ELSE IF(ERR.EQ.-3) THEN
C            didn't close near the lower limit, no possibility of
C            high head flow, either super critical in approach section
C            or a spatial convergence problem has occurred.
            ERR=-4
          ENDIF
Cread     READ(*,*)IXXXK
          ERRCOD(INUM)=ERR
          H1E(INUM)=-1
          D2(INUM)= -1
          D3A(INUM)=-1
        ENDIF
      ENDIF
 999  CONTINUE
Cread      READ(*,*)IXXK
C
      RETURN
      END
C
C=====TYPTHRE bof=============================================================
C
C=====INLTD3 bof==============================================================
C-----Purpose:
C       Computes the energy equation between the inlet and outlet of
C       the culvert.  Used to determine the depth at the culvert inlet
C       for type 3 flow.
C       Equation used is in TWRI Chapter A3, "Measurements of Peak
C       Discharge at Culverts by Indirect Methods",pg 30.
C     Programmed by: JM Fulford
C     Date:
C     Modified by:
C     Last modified:
C
      REAL FUNCTION INLTD3
     I                (H2,ERR)
C
C-----Arguments:
      INTEGER ERR
      REAL H2
C
C-----Argument definitions:
C
C-----Module data:
      INCLUDE 'FCNCN2.INC'
C
C-----Local variables:
      REAL A2,K2,T0
C
C-----Externals:
      EXTERNAL CULINT
C
      ERR=0
      CALL CULINT (H2,A2,K2,T0,ERR)
      IF(ERR.EQ.0.)THEN
      INLTD3 = D3 +VGD3 +H23D3/K2 -H2 -(QG/(A2*A2)) -Z
      ENDIF
C
      RETURN
      END
C
C=====INLTD3 eof==============================================================
C
C=====NORDEP bof==============================================================
C-----Purpose:
C       Computes the normal depth at a given discharge of the culvert.
C       Used to determine the depth at the culvert inlet when culvert slope
C       is mild and ERR =-3 from RTMIA for type 3 flow.
C     Programmed by: JM Fulford
C     Date:
C     Modified by:
C     Last modified:
C
      REAL FUNCTION NORDEP
     I                (H2,ERR)
C
C-----Arguments:
      INTEGER ERR
      REAL H2
C
C-----Argument definitions:
C
C-----Module data:
      INCLUDE 'FCNCN2.INC'
      INCLUDE 'CULFLW.INC'
C
C-----Local variables:
      REAL A2,K2,T0
C
C-----Externals:
      EXTERNAL CULINT
C
      ERR=0
      CALL CULINT (H2,A2,K2,T0,ERR)
      IF(ERR.EQ.0.)THEN
        NORDEP = (CQ*CQ)-(K2*K2*Z)/L
      ENDIF
C
      RETURN
      END
C
C=====NORDEP eof==============================================================
C
C=====TYP3FW bof==============================================================
C-----Purpose:
C       Computes the energy equation for type 3 culvert flow.
C       Equation used is in TWRI Chapter A3, "Measurements of Peak
C       Discharge at Culverts by Indirect Methods",pg 2.
C     Programmed by: JM Fulford
C     Date:
C     Modified by:
C     Last modified:
C
      REAL FUNCTION TYP3FW
     I                (H1,ERR)
C
C-----Arguments:
      REAL H1
C
C-----Module data:
      INCLUDE 'FCNCN2.INC'
C
C-----Local variables:
      INTEGER ERR,TYPFW
      REAL C,A1,K1,APH1,W
C
C-----Externals
      REAL CADJST
      EXTERNAL CADJST,KINTER
C
      TYPFW=3
      CALL KINTER(H1,A1,K1,W,APH1,ERR)
      CC=CADJST(H1,AD3,A1,TYPFW)
      IF(CC.EQ.0)ERR=3
      IF(ERR.EQ.0) THEN
      C=1.0/CC
      TYP3FW = D3 +H23D3 +C*C*VGD3 +H12D2/K1 -H1 -APH1*QG/(A1*A1)
C     following commented out lines compute entry/exit losses using
C     the velocity at the culvert inlet instead of the exit
C      TYP3FW = D3 + H23D3 + VGD3 +H12D2/K1 -H1 -APH1*QG/(A1*A1)
C     #  + (C*C-1.)*VGD2
      ENDIF
C
      RETURN
      END
C
C=====TYP3FW eof==============================================================
C
C=====TYP3SW bof==============================================================
C-----Purpose:
C       Computes the energy equation for type 3 culvert flow.
C       Equation used is in TWRI Chapter A3, "Measurements of Peak
C       Discharge at Culverts by Indirect Methods",pg 2.
C       modified from typ3fw to used shorten lw to approach and
C       translates the approach section geometery downstream.
C     Programmed by: JM Fulford
C     Date:
C     Modified by:
C     Last modified:
C
      REAL FUNCTION TYP3SW
     I                (H1,ERR)
C
C-----Arguments:
      REAL H1
C
C-----Module data:
      INCLUDE 'FCNCN2.INC'
      INCLUDE 'APPROC.INC'
C
C-----Local variables:
      INTEGER ERR,TYPFW
      REAL C,AA1,KA1,AAPH1,WA,MH1
C
C-----Externals
      REAL CADJST
      EXTERNAL CADJST,KINTER
C
      TYPFW=3
      MH1=H1+0.5*(W1(1)+Z)
      CALL KINTER(MH1,AA1,KA1,WA,AAPH1,ERR)
      CC=CADJST(MH1,AD3,AA1,TYPFW)
      IF(CC.EQ.0)ERR=3
      IF(ERR.EQ.0) THEN
      C=1.0/CC
      TYP3SW = D3 +H23D3 +C*C*VGD3 +H12D2/KA1 -H1 -AAPH1*QG/(AA1*AA1)
C     following commented out lines compute entry/exit losses using
C     the velocity at the culvert inlet instead of the exit
C      TYP3FW = D3 + H23D3 + VGD3 +H12D2/K1 -H1 -APH1*QG/(A1*A1)
C     #  + (C*C-1.)*VGD2
      ENDIF
C
      RETURN
      END
C
C=====TYP3SW eof==============================================================
C
C=====TYPFOUR bof==============================================================
C-----Purpose:
C       Computes the energy equation for type 4 culvert flow.  The equation
C       includes friction losses and the velocity head in the approach
C       section.  It is similar to the equation for type 3 except that the
C       pipe is assumed to flow full for the entire length.
C     Programmed by:  JM Fulford
C     Date:
C     Modified by:
C     Last modified:
C
      SUBROUTINE TYPFOUR (TW,ERR)
C
C-----Dummy arguments:
      INTEGER ERR
      REAL TW
C
C-----Module data:
      INCLUDE 'FCNCN2.INC'
      INCLUDE 'CULFLW.INC'
C
C-----Local variables:
      INTEGER I4
      REAL KD3,A3,TO,LPART
C
C-----Externals:
      REAL H4EST,TYP4FW,TYP3LEN
      EXTERNAL H4EST,TYP4FW,TYP3LEN,CULINT,TYPTHRE
C
C
Cwrite      WRITE(*,*)'IN TYPFOUR'
      I4=0
      D3=TW
      HA=D+Z
      IF(D.LT.HA) HA=D
      HB=(H4EST()+TW)*1.2
 10   CONTINUE
      F=TYP4FW(HA,ERR)
Cwrite      WRITE(*,*)'TYPFOUR HA F',HA,F,ERR
      F=TYP4FW(HB,ERR)
Cwrite      WRITE(*,*)'TYPFOUR HB F',HB,F,ERR
      CALL RTMIA(H1E(INUM),F,TYP4FW,HA,HB,TOL,ISTEPS,ERR)
Cwrite      WRITE(*,*)'ERR IN TYPFOUR AFTER RTMIA',ERR
Cwrite      READ(*,*)IIXXX
      IF(H1E(INUM).LT.(D+Z).AND.ERR.EQ.0) ERR=-3
      IF(ERR.EQ.-2.AND.I4.LE.5) THEN
C       root not bracketed, nearest upper bracket hb
C       increases the bracket and tries again
Cwrite        WRITE(*,*)'Error in type four calculation',ERR
        ERRCOD(INUM)=ERR
        HB=HB*1.2
        I4=I4+1
      ELSE IF (ERR.EQ.-3) THEN
C       root not bracketed, nearest lower bracket, ha
C       or inlet not submerged.  Culvert flows full and
C       part full in inlet.
        IF(TYPE(INUM).EQ.43) WRITE(*,*)'WARNING--possible error',
     #  ' computing from type 43 to 34'
        TYPE(INUM)=34
        D3A(INUM)=D
        LPART=TYP3LEN(D,TW)
        CALL CULINT(D3A(INUM),AD3,KD3,TO,ERR)
Cwrite        WRITE(*,*)'CULINT TYPFOUR A3=0',D3A(INUM)
        IF(ERR.EQ.0) THEN
           H23D3=LPART*CQ*CQ/KD3
           VGD3=QG/(AD3*AD3)
           TW=D+(L-LPART)*Z/L
           CALL TYPTHRE(TW,ERR)
           IF(ERR.EQ.0) ERRCOD(INUM)=13
           D3A(INUM)=D
        ENDIF
      ELSE IF(ERR.EQ.0) THEN
        D3A(INUM)=D
        IF(TW.LT.D) D3A(INUM)=TW
        ERRCOD(INUM)=0
        H1E(INUM)=H1E(INUM)+BASEL
        PKON(INUM)=CC
      ELSE
        H1E(INUM)=-1
        D2(INUM)=-1
      ENDIF
      IF(ERR.EQ.-2.AND.I4.LT.5) GO TO 10
C
      RETURN
      END
C
C=====TYPFOUR bof==============================================================
C
C=====TYP4FW bof==============================================================
C-----Purpose:
C       Computes the energy equation for type 4 culvert flow.  Includes
C       approach velocity head and friction losses in the approach reach.
C     Programmed by: JM Fulford
C     Date:
C     Modified by:
C     Last modified:
C
      REAL FUNCTION TYP4FW
     I                    (H1,ERR)
C
C-----Arguments:
      REAL H1
      INTEGER ERR
C
C-----Arguments definitions:
C     H1    - approach water surface elevation reference to downstream
C             culvert invert elevation
C     ERR   - error code
C
C-----Module data:
      INCLUDE 'FCNCN2.INC'
      INCLUDE 'CULFLW.INC'
C
C-----Local variables:
      REAL A1,K1,W,APH1
C
      CALL KINTER (H1,A1,K1,W,APH1,ERR)
      IF(ERR.EQ.0) THEN
        IF(A1.EQ.0) THEN
Cwrite          WRITE(*,*)'A1=0 IN TYP4FW',H1
          ERR=1
        ELSE
          TYP4FW = H23D3 + H12D2/K1 - H1 - (APH1*QG/(A1*A1))
        ENDIF
      ELSE
Cwrite        WRITE(*,*)'ERR IN KINTER, TYP4FW',ERR
Cwrite        READ(*,*)IXXX
      ENDIF
C
      RETURN
      END
C
C=====TYP4FW bof==============================================================
C
C=====H4EST bof============================================================
C
C-----Purpose:
C       Computes the energy equation for type 4 culvert flow.
C       Equation used is in TWRI Chapter A3, "Measurements of Peak
C       Discharge at Culverts by Indirect Methods",pg 2.
C     Programmed by: JM Fulford
C     Date:
C     Modified by:
C     Last modified:
C
      REAL FUNCTION H4EST ()
C
C-----Arguments:
C       none
C
C-----Module data:
      INCLUDE 'CULVRT.INC'
      INCLUDE 'FCNCN2.INC'
C
C-----Local variables:
      REAL KO,AO,CON2,CON3,CON4
C
      KO=CULK(TBSIZE)
      AO=CULA(TBSIZE)
      CON2=1.0/(C46*C46)-1.0
      CC=C46
      CON3=0.01552795*(CQ/AO)**2
      CON4=CLEN*CQ*CQ/(KO*KO)
      H4EST=CON2*CON3+CON4+CON3
C
      RETURN
      END
C
C=====H4EST eof============================================================
C
C=====GETC46 bof================================================================
C-----Purpose:
C       Returns c46, discharge coefficient for flow types 4 and 6.
C
      REAL FUNCTION GETC46 ()
C
C-----Module data:
      INCLUDE 'CULVRT.INC'
C
      GETC46=C46
C
      RETURN
      END
C
C=====GETC46 eof================================================================
C
C=====TYP5FW bof==============================================================
C-----Purpose:
C       Computes the energy equation for type 5 culvert flow.
C       Equation used is in TWRI Chapter A3, "Measurements of Peak
C       Discharge at Culverts by Indirect Methods",pg 2.
C     Programmed by: JM Fulford
C     Date:
C     Modified by:
C     Last modified:
C
      REAL FUNCTION TYP5FW
     I                (H1,ERR)
C
C-----Arguments:
      REAL H1
      INTEGER ERR
C
C-----Argument definitions:
C
C-----Module data:
      INCLUDE 'FCNCN2.INC'
      INCLUDE 'CULFLW.INC'
C
C-----Local variables:
      REAL C
C
C-----Externals:
      REAL C5INT
      EXTERNAL C5INT
C
C
      ERR=0
      C=C5INT(H1)
      CC=C
      TYP5FW = Z -H1 + VGD3/(C*C)
C
      RETURN
      END
C
C=====TYP5FW eof==============================================================
C
C=====TYP6FW bof============================================================
C
C-----Purpose:
C       Computes the energy equation for type 6 culvert flow.
C       Equation used is in TWRI Chapter A3, "Measurements of Peak
C       Discharge at Culverts by Indirect Methods",pg 2.
C     Programmed by: JM Fulford
C     Date:
C     Modified by:
C     Last modified:
C
      REAL FUNCTION TYP6FW ()
C
C-----Arguments:
C       none
C
C-----Module data:
      INCLUDE 'CULVRT.INC'
      INCLUDE 'FCNCN2.INC'
C
C-----Local variables:
      REAL HR43,X,KF1,KF2,KF,CONST
C
      HR43=(NRUFF*CULK(TBSIZE)/(1.486*CULA(TBSIZE)))**2
      X=29.0*CLEN*NRUFF*NRUFF/HR43
C      KF1=1.222081+X*(-0.39096055+X*(0.10127278-X*0.010547583))
C      KF2=1.6475101+X*(-0.56519116+X*(0.12983789-X*0.011096691))
      KF1=1.2002+X*(-0.37526+X*(0.13410+X*(-.034714+X*(0.0047746
     #    -X*0.00023996))))
      KF2=1.7713+X*(-0.93433+X*(0.48185+X*(-0.14896+X*(0.02349
     #    -X*0.0014455))))
      CC=C46
      IF (CC.LE.0.76) THEN
        KF=KF1
      ELSE IF (CC.GE.1.0) THEN
        KF=KF2
      ELSE
        KF=KF1+4.166666*(CC-0.76)*(KF2-KF1)
      ENDIF
      D=CULD(TBSIZE)
      CONST=CQ/(CULA(TBSIZE)*KF*SQRT(D))
      TYP6FW=1.0993391+CONST*(-0.1032313+CONST*
     #       (0.055182648-CONST*0.00098596166))
C
      RETURN
      END
C
C=====TYP6FW eof============================================================
C
C=====TYP3LEN bof============================================================
C
C-----Purpose:
C     Computes the length of culvert in which type 3 flow occurs
C     by using the type 4 flow equation, assuming C46=1. and the
C     velocity head in not zero.  See TWRI Chapter A3, "Measurements
C     of Peak Discharge at Culverts by Indirect Methods",pg 2.
C     Programmed by: JM Fulford
C     Date:
C     Modified by:
C     Last modified:
C
      REAL FUNCTION TYP3LEN (D,HTW)
C
C-----Arguments:
      REAL D,HTW
C
C-----Arguments definitons:
C     D    - culvert height
C
C-----Module data:
      INCLUDE 'CULVRT.INC'
      INCLUDE 'FCNCN2.INC'
C
C-----Local variables:
      REAL KO,CON4
C
      KO=CULK(TBSIZE)
      CON4=Z/CLEN - (CQ/KO)**2
      TYP3LEN=CLEN-(HTW-D)/CON4
C
      RETURN
      END
C
C=====TYP3LEN eof============================================================
C
C=====TYP4LEN bof============================================================
C
C-----Purpose:
C     Computes the length of culvert in which type 4 flow occurs
C     by using the type 3 flow equation with c=1. See TWRI
C     Chapter A3, "Measurements
C     of Peak Discharge at Culverts by Indirect Methods",pg 2.
C     Programmed by: JM Fulford
C     Date:
C     Modified by:
C     Last modified:
C
      REAL FUNCTION TYP4LEN (D,HTW)
C
C-----Arguments:
      REAL D,HTW
C
C-----Arguments definitons:
C     D    - culvert height
C
C-----Module data:
      INCLUDE 'CULVRT.INC'
      INCLUDE 'FCNCN2.INC'
C
C-----Local variables:
      REAL KO,CON4,K3,A3,AO,TO
C
      CALL CULINT(D,AO,KO,TO,IERR)
      CALL CULINT(HTW,A3,K3,TO,IERR)
Cwrite      WRITE(*,*)'D,HTW',D,HTW,IERR
      CON4=Z/CLEN
      CON4=CON4 - (CQ*CQ)/(KO*K3)
      TYP4LEN=CLEN-(HTW-D+ QG/(A3*A3) - QG/(AO*AO))/CON4
C
      RETURN
      END
C
C=====TYP4LEN eof============================================================
C
C=====CADJST bof============================================================
C
C-----Purpose:
C       Adjusts the culvert discharge coefficient according to
C       equation in TWRI Chapter A3, "Measurements of Peak
C       Discharge at Culverts by Indirect Methods",pg 38 and
C       inverts the coefficient.  For type 1,2&3 flows.
C       returns zero when it is unable to interpolate a value
C     Programmed by: JM Fulford
C     Date:
C     Modified by: gfkoltun for 2021 version
C     Last modified:
C
      REAL FUNCTION CADJST
     I               (H1,AC,A1,TYPFW)
C
C-----Arguments:
      INTEGER TYPFW
      REAL AC,A1,H1
C
C-----Module data:
      INCLUDE 'CULVRT.INC'
      INCLUDE 'FCNCN2.INC'
      INCLUDE 'CULFLW.INC'
C
C-----Local variables:
      INTEGER N,J,FLG
      REAL C,H
C
      H=(H1-ZDROP)/D
Cwrite      WRITE(*,*)'TC, QG, D3',TC,QG,D3,NHP,A1,AC
C     Test below is true if coeff and height data supplied on *C1
      IF(NHP.GE.1) THEN
        N=NHP-1
        J=0
        FLG=0
10      CONTINUE
          J=J+1
          IF (H.GE.HP(J).AND.H.LE.HP(J+1)) FLG=1
        IF(FLG.NE.1.AND.J.LT.N) GO TO 10
        IF(FLG.EQ.1)THEN
          C=CP(J)+(H-HP(J))*(CP(J+1)-CP(J))/
     #      (HP(J+1)-HP(J))
        ELSE IF (H.LE.HP(1)) THEN
          C=CP(1)
        ELSE IF (J.GE.N) THEN
          C=CP(N)
        ELSE
          C=0
        ENDIF
      ELSE
C       determine coefficients from graphs in TWRI
Cwrite        WRITE(*,*)'IN CADJST'
C        Test below is true for box culverts
         IF(TC.GT.9.AND.TC.LT.20) THEN
C         determine discharge coefficient for box culverts
          IF(TYPFW.LT.3)THEN
C           Do this if flow type = 1 or 2
            IF(NHP.EQ.0)THEN
C             If *C1 specified
              C=CB12
            ELSE
              C=0.95
            ENDIF
          ELSE
C           Do this if flow type equal 3
C           box culverts - equation for fig.23 in TWRI (checked by gfk)
            F=CQ/(AC*SQRT(D3*32.2))
            C=0.71364867+F*(0.38017909-0.14345278*F)
Cread       READ(*,*)IXXK
          ENDIF
C         Don't apply coefficient if flow type = 1 or 2, *C1 card used
          IF ((TYPFLW.LE.2).AND.(NHP.GE.0)) THEN
C           Don't alter C
          ELSE
            C=C*KWR*KWING
          ENDIF
          IF(C.GE.0.98) C=0.98
        ELSE
C         Use discharge coefficients for H=0.4 when H<0.4 TWRI figs. 20 and 25.
C         This prevents interpolation to values smaller than indicated mins.
          IF(H.LT.0.4) H=0.4
C         determine discharge coefficient for pipe & pipe archs culverts
C         Note that KPROJ previously set to 1.0 for all concrete pipes
          IF(INLET.EQ.3.OR.INLET.EQ.4)THEN
C           tongue & groove or bellmouth concrete pipe
            C=0.95
          ELSE IF(INLET.EQ.2)THEN
C           mitered entrance, flush with sloping embankment fig.25 TWRI
            C=0.73620+(0.54049-0.49769*H+0.089097*H*H)*H
            C=C*KPROJ
          ELSE ! inlet=1
C           pipe culverts with square entrance, flush with vertical headwall
            C=0.88821+(0.21047-0.29299*H+0.078988*H*H)*H 
            C=C*KWR*KPROJ
          ENDIF
        ENDIF
      ENDIF
C     adjustment for channel contraction for types 1-3 flow
      IF(TYPFW.LE.3)THEN
C     Note: equation is eqn from TWRI B3-CA3 pg38 written in terms of the culvert area
C           (AC) and the approach area (A1)
      IF(AC.GE.0.2*A1.AND.A1.NE.0.0) C=1.25*(C-0.196+(AC*(0.98-C)/A1))
Cread      READ(*,*)IXXK
      ENDIF
      IF(C.GT.0.98) C=0.98
      CADJST=C
C
      RETURN
      END
C
C=====CADJST eof============================================================
C
C=====WINGWALL bof==============================================================
C
C-----Purpose:
C       computes the discharge coefficient adjustment due to the
C       wingwall angle. Computed when the value of angle is read.
C     Programmed by: JM Fulford
C     Date:
C     Modified by:
C     Last modified:
C
      REAL FUNCTION WINGWALL (THETA)
C
C-----Arguments:
      REAL THETA
C
C-----Local variables:
      REAL ANG
C
      IF(THETA.LT.0.0.OR.THETA.GT.90)THEN
        WINGWALL=-1.0
      ELSE IF (THETA.EQ.0) THEN
        WINGWALL=1.0
      ELSE
C       Convert theta to radians
        THETA=THETA/57.2958
        ANG=COS(THETA)
C       Following calculation approximates fig. 24
        IF(ANG.LT.0.5)THEN
          WINGWALL=1.0594+0.3062*ANG
        ELSE
          WINGWALL=1.2402+(0.27173-0.79619*ANG+0.28426*ANG*ANG)*ANG
        ENDIF
      ENDIF
C
      RETURN
      END
C
C=====WINGWALL eof============================================================
C=====C5INT bof==============================================================
C
C-----Purpose:
C       Interpolates discharge coefficient for type 5 flow in culverts.
C     Programmed by: JM Fulford
C     Date:
C     Modified by:
C     Last modified:
C
      REAL FUNCTION C5INT (H1)
C
C-----Arguments
      REAL H1
C
C-----Module data:
      INCLUDE 'CULVRT.INC'
      INCLUDE 'CULFLW.INC'
C
C-----Local variables:
      INTEGER TYPFW,N,FLG,J
      REAL H,CTEMP
C
C-----Externals
      REAL C5TAB7,C5TAB6,C5FLARED
      EXTERNAL C5TAB7,C5TAB6,C5FLARED
C
      TYPFW=5
      H=(H1-ZDROP)/D
      IF(NH5.GT.0)THEN
C       compute using input values from input data file 
      N=NH5-1
      FLG=0
      J=0
1     CONTINUE
      J=J+1
      IF(H5(J).LE.H.AND.H.LE.H5(J+1)) FLG=1
      IF(ABS(H5(J)-H).LE.0.00015.OR.ABS(H5(J+1)-H).LE.0.00015) FLG=1
      IF(FLG.NE.1.AND.J.LT.N) GO TO 1
      IF(FLG.NE.1) THEN
C        WRITE (*,*) TYPFW,H
        IF (H.GT.H5(NH5)) C5INT=C5(NH5)
        IF (H.LT.H5(1)) C5INT=C5(1)
      ELSE
        C5INT=C5(J)+(C5(J+1)-C5(J))*(H-H5(J))/(H5(J+1)-H5(J))
      ENDIF
C
      ELSE
C       compute default values based on TWRI tables
        IF(INLET.EQ.2)THEN
          C5INT=0.92
        ELSE IF(INLET.NE.4)THEN
          IF(TC.EQ.1)THEN
            C5INT=C5TAB7(THETA,H)
            IF(RND.GT.0.0) THEN
              CTEMP=C5TAB6(RND,H)
              IF(CTEMP.GT.C5INT) C5INT=CTEMP
            ENDIF
          ELSE
            C5INT=C5TAB6(RND,H)              
          ENDIF
        ELSE IF (INLET.EQ.4) THEN
          C5INT=C5FLARED(H)
        ENDIF
        IF(INLET.NE.4)THEN
          C5INT=KPROJ*C5INT
        ENDIF 
      ENDIF
C
      RETURN
C
    3 FORMAT (' ',2X,'TYPE..',I2,5X,'H NOT IN THE RANGE OF H5...H=',F8.
     14)
      END
C
C=====C5INT bof==============================================================
C
C=====C5TAB7 bof=============================================================
C
C-----Purpose:
C       interpolate from tabled values C5 using table 7 of the TWRI on
C       culverts
C     Programmed by: JM Fulford
C     Date: 12.96
C
      REAL FUNCTION C5TAB7(THETA,H)
C
C     Arguments:
      REAL THETA,H
C
C     Local variables
      INTEGER I,J
      REAL TNODE(6),HNODE(13),C5VALS(6,13)
      REAL LX,LY,A1,B2
C
C-----Initializations
      DATA C5VALS /
     # 0.44,0.44,0.44,0.43,0.42,0.39,
     # 0.44,0.46,0.46,0.45,0.43,0.41,
     # 0.46,0.47,0.47,0.46,0.45,0.42,
     # 0.47,0.49,0.49,0.48,0.46,0.43,
     # 0.48,0.50,0.50,0.48,0.47,0.44,
     # 0.49,0.51,0.51,0.50,0.48,0.45,
     # 0.50,0.52,0.52,0.51,0.49,0.46,
     # 0.51,0.53,0.53,0.52,0.49,0.46,
     # 0.54,0.56,0.56,0.54,0.52,0.49,
     # 0.55,0.58,0.58,0.56,0.54,0.50,
     # 0.57,0.60,0.60,0.58,0.55,0.52,
     # 0.58,0.61,0.61,0.59,0.56,0.53,
     # 0.59,0.62,0.62,0.60,0.58,0.54  /
      DATA TNODE / 0.0, 30., 45., 60., 75., 90. /
      DATA HNODE /
     #     1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2., 
     #     2.5, 3.0, 3.5, 4.0, 5.0 / 

C
      I=0
 10   CONTINUE
      I=I+1
      IF(TNODE(I).LT.THETA.AND.I.LT.6) GO TO 10
      J=0
 20   CONTINUE
      J=J+1
      IF(HNODE(J).LT.H.AND.J.LT.13) GO TO 20
C
      IF(I.EQ.1)THEN
        IF(J.GT.1.AND.J.LT.14)THEN
          X1=C5VALS(I,J-1)
          X2=C5VALS(I,J)
        ELSE 
          IF(J.GT.13) THEN
            X1=C5VALS(I,13)
          ELSE
            X1=C5VALS(I,J)
          ENDIF
          X2=X1
        ENDIF
      ELSE IF(I.GT.6) THEN
        IF(J.LT.14.AND.J.GT.1)THEN
          X1=C5VALS(6,J-1)       
          X2=C5VALS(6,J)
        ELSE
          IF(J.GT.13)THEN
            X1=C5VALS(6,13)
          ELSE
            X1=C5VALS(6,J)
          ENDIF
        ENDIF
      ELSE
        LX=TNODE(I) - TNODE(I-1)
        A1=(THETA - TNODE(I-1))/LX
        IF(J.GT.1.AND.J.LE.13)THEN
          X1=C5VALS(I-1,J-1) + A1*(C5VALS(I,J-1) - C5VALS(I-1,J-1))
          X2=C5VALS(I-1,J) + A1*(C5VALS(I,J) - C5VALS(I-1,J))
        ELSE IF(J.LE.1)THEN
          X1=C5VALS(I-1,J) + A1*(C5VALS(I,J) - C5VALS(I-1,J))
          X2=X1
        ELSE 
          X1=C5VALS(I-1,13) + A1*(C5VALS(I,13) - C5VALS(I-1,13))
          X2=X1
        ENDIF
      ENDIF
      IF(J.EQ.1)THEN      
        B2=0
      ELSE IF(J.GT.13)THEN
        B2=0
      ELSE
        LY=HNODE(J)-HNODE(J-1)
        B2=(H - HNODE(J-1))/LY
      ENDIF
      C5TAB7=X1 + B2*(X2-X1)
C
      RETURN
      END 
C
C=====C5TAB7 eof=============================================================
C
C=====C5TAB6 bof=============================================================
C
C-----Purpose:
C      Interpolate from table 6 values of twri on culverts to determine C5
C     Programmed by: JM Fulford
C     Date: 12.96
C
      REAL FUNCTION C5TAB6(RND,H)
C
C-----Arguments:
      REAL RND,H
C
C-----Local Variables:
      INTEGER I,J
      REAL RNODE(7),HNODE(12),C5VALS(7,12)
      REAL LX,LY,A1,B2,X1,X2
C
C-----Initializations
      DATA C5VALS /
     #  0.44, 0.46, 0.49, 0.50, 0.50, 0.51, 0.51,
     #  0.46, 0.49, 0.52, 0.53, 0.53, 0.54, 0.54,
     #  0.47, 0.51, 0.54, 0.55, 0.55, 0.56, 0.56,
     #  0.48, 0.52, 0.55, 0.57, 0.57, 0.57, 0.57,
     #  0.49, 0.54, 0.57, 0.58, 0.58, 0.58, 0.58,
     #  0.50, 0.55, 0.58, 0.59, 0.60, 0.60, 0.60,
     #  0.51, 0.56, 0.59, 0.60, 0.61, 0.61, 0.62,
     #  0.54, 0.59, 0.62, 0.54, 0.64, 0.65, 0.66,
     #  0.55, 0.61, 0.64, 0.66, 0.67, 0.69, 0.70,
     #  0.57, 0.62, 0.65, 0.67, 0.69, 0.70, 0.71,
     #  0.58, 0.63, 0.66, 0.68, 0.70, 0.71, 0.72,
     #  0.59, 0.64, 0.67, 0.69, 0.71, 0.72, 0.73 / 
      DATA RNODE/ 0, 0.02, 0.04, 0.06, 0.08, 0.10, 0.14 /
      DATA HNODE /
     #      1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2.0, 
     #      2.5, 3.0, 3.5, 4.0, 5.0 / 
C
      I=0
 10   CONTINUE
      I=I+1
      IF(RNODE(I).LT.RND.AND.I.LT.7) GO TO 10
      J=0
 20   CONTINUE
      J=J+1
      IF(HNODE(J).LT.H.AND.J.LT.12) GO TO 20
C
      IF(I.EQ.1)THEN
        IF(J.GT.1.AND.J.LE.12)THEN
          X1=C5VALS(I,J-1)
          X2=C5VALS(I,J)
        ELSE
          IF(J.GT.12) THEN
            X1=C5VALS(I,12)
          ELSE
            X1=C5VALS(I,J)
          ENDIF
          X2=X1
        ENDIF
      ELSE IF(I.GT.7) THEN
        IF(J.LT.13.AND.J.GT.1)THEN
          X1=C5VALS(7,J-1)
          X2=C5VALS(7,J)
        ELSE
          IF(J.GT.12)THEN
            X1=C5VALS(7,12)
          ELSE
            X1=C5VALS(7,J)
          ENDIF
          X2=X1
        ENDIF
      ELSE
        LX=RNODE(I) - RNODE(I-1)
        A1=(RND - RNODE(I-1))/LX
        IF(J.GT.1.AND.J.LE.12)THEN
          X1=C5VALS(I-1,J-1) + A1*(C5VALS(I,J-1) - C5VALS(I-1,J-1))
          X2=C5VALS(I-1,J) + A1*(C5VALS(I,J) - C5VALS(I-1,J))
        ELSE IF(J.LE.1)THEN
          X1=C5VALS(I-1,1) + A1*(C5VALS(I,1) -C5VALS(I-1,1))
          X2=X1
        ELSE
          X1=C5VALS(I-1,12) + A1*(C5VALS(I,12)-C5VALS(I-1,12))
          X2=X1
        ENDIF
      ENDIF
      IF(J.EQ.1)THEN      
        B2=0
      ELSE IF(J.GT.12)THEN
        B2=0
      ELSE
        LY=HNODE(J)-HNODE(J-1)
        B2=(H - HNODE(J-1))/LY
      ENDIF
      C5TAB6=X1 + B2*(X2-X1)
C
      RETURN
      END 
C
C=====C5TAB6 bof=============================================================
C
C=====C5FLARED eof===========================================================
C
C-----Purpose:
C       interpolate tabled values of C5 for flared pipe ends from table on
C       page 44 of the twri.
C     Programmed by: JM Fulford
C     Date: 12.96
C
      REAL FUNCTION C5FLARED(H)
c
C-----Arguments:
      REAL H
C
C-----Local variables:
      INTEGER I,NI
      REAL HNODE(12),C5VALS(12),LX
C
C-----Initializations
      DATA C5VALS /
     # 0.48, 0.50, 0.52, 0.53, 0.55, 0.56, 0.57, 0.59
     #,0.61, 0.63, 0.65, 0.66 /
      DATA HNODE /
     #  1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2.0, 2.5, 3.0,
     #  3.5, 4.0, 5.0 /
C
      I=0
      NI=12
 10   CONTINUE
      I=I+1
      IF(HNODE(I).LT.H.AND.I.LT.NI) GO TO 10
C
      IF(I.EQ.1)THEN
        C5FLARED=C5VALS(I)
      ELSE IF(I.EQ.NI.AND.H.GE.HNODE(I))THEN
        C5FLARED=C5VALS(I)
      ELSE
        X=H- HNODE(I-1)
        LX=HNODE(I) - HNODE(I-1)
        C5FLARED = C5VALS(I-1) + X*C5VALS(I)/LX
      ENDIF
C 
      RETURN
      END
C
C=====C5FLARED bof===========================================================
C=====H1EST bof==============================================================
C
C-----Purpose:
C       interpolate estimated approach elevation from cq
C     Programmed by: JM Fulford
C     Date:
C     Modified by:
C     Last modified:
C
      REAL FUNCTION H1EST (CQ)
C
C-----Parameters:
      REAL CQ
C
C-----Module data:
      INCLUDE 'APPROC.INC'
C
C-----Local variables:
      INTEGER J,FLG
C
      J=0
      FLG=0
 10   CONTINUE
        J=J+1
        IF(QC(J).LE.CQ.AND.CQ.LE.QC(J+1)) FLG=1
      IF(FLG.NE.1.AND.J.LT.NQW-1) GO TO 10
      IF (FLG.EQ.1)THEN
        H1EST=W1(J)+(W1(J+1)-W1(J))*(CQ-QC(J))/(QC(J+1)-QC(J))
      ELSE
        H1EST=0.
      ENDIF
C     note for the following case, flow must be tranquil in approach
      IF(H1EST.LT.0.0) H1EST=0.0
C
      RETURN
      END
C
C=====H1EST eof==============================================================
C
C=====H1CMP bof==============================================================
C
C-----Purpose:
C       compute estimated approach elevation by computing
C       the critcial discharge in the approach section using
C       interpolated values of area and wetted perimeter.  Should
C       be a better estimate than linear interpolation.  Assumes
C       discharge is zero at water surface elevation in the approach
C       equal to culvert drop, z.
C
C     Programmed by: JM Fulford
C     Date:
C     Modified by:
C     Last modified:
C
      REAL FUNCTION H1CMP (Z,ERR)
C
C-----Parameters:
      INTEGER ERR
      REAL Z
C
C-----Module data:
      INCLUDE 'APPROC.INC'
C
C-----Local variables:
      INTEGER ISTEPS
      REAL HLOW,HHIGH,TOL,F,HC
C
C-----Externals:
      REAL WSCRITQ
      EXTERNAL WSCRITQ
C
      HLOW=Z
      HLOW=HLOW+0.001
      IF(HLOW.LT.W1(1)) HLOW=W1(1)+0.001
      HHIGH=W1(25)
      TOL=0.0001
      ISTEPS=50
      CALL RTMIA(HC,F,WSCRITQ,HLOW,HHIGH,TOL,ISTEPS,ERR)
      IF(ERR.EQ.0)THEN
        H1CMP=HC
      ELSE IF(ERR.EQ.-3) THEN
        H1CMP=Z
        ERR=0
      ELSE
Cwrite        WRITE(*,*)'ERROR IN H1CMP ERR=',ERR
        H1CMP=-999
      ENDIF
C
      RETURN
      END
C
C=====H1CMP eof==============================================================
C
C=====WSCRITQ bof==============================================================
C
C-----Purpose:
C       compute estimated approach elevation by computing
C       the critcial discharge in the approach section using
C       interpolated values of area and wetted perimeter.  Should
C       be a better estimate than linear interpolation.
C
C     Programmed by: JM Fulford
C     Date:
C     Modified by:
C     Last modified:
C
      REAL FUNCTION WSCRITQ (HC,ERR)
C
C-----Parameters:
      REAL HC
      INTEGER ERR
C
C-----Module data:
      INCLUDE 'APPROC.INC'
      INCLUDE 'FCNCN2.INC'
C
C-----Local variables:
      REAL A,B,K,AAPH
C
      CALL KINTER(HC,A,K,B,AAPH,ERR)
      IF(ERR.NE.0) WRITE(*,*)'WSCRITQ KINTER HC, ERR',HC,ERR
      WSCRITQ = B*CQ*CQ - A*A*A*32.2
C
C
      RETURN
      END
C
C=====WSCRITQ eof==============================================================
C
C
C=====WCULTB bof==============================================================
C
C-----Purpose:
C       print out culvert section properties for culvert program
C     Programmed by: JM Fulford
C     Date:
C     Modified by: gfkoltun for 2021 version
C     Last modified:
C
      SUBROUTINE WCULTB(OTNIT,PNO,HTITLE)
C
C-----Arguments:
      INTEGER OTNIT,PNO
      CHARACTER*80 HTITLE
C
C-----Module data:
      INCLUDE 'CULVRT.INC'
      INCLUDE 'CULFLW.INC'
C
C-----PARAMETER
      INTEGER NPAGE
      PARAMETER (NPAGE=60)
C
C-----Local variables:
      INTEGER I,IPAGE,ISTART,NOI,TCR,PC
      CHARACTER*15 CTYPE(6)
      REAL INCH_TO_CM
C
C-----Data initializations:
      DATA CTYPE /
     #   'box/rectangular',
     #   'circular/pipe  ',
     #   'pipe-arch      ',
     #   'nonstandard    ',
     #   'vert. ellipse  ',
     #   'hor. ellipse   '/
C
C-----Formats:
  100 FORMAT (A80,/,15X,'CULVERT SECTION PROPERTIES - ID: ',A16)
  101 FORMAT (/,20X,'Culvert section type: ',A15)
  102 FORMAT (15X,'(r or w)/D  KR or KW  Ktheta  Kproj    n      Inlet')
  103 FORMAT (19X,F4.2,6X,3(F4.2,3X),F5.3,5X,I3)
  104 FORMAT (/,9X,'Barrel',36X,'Top',8X,'Wetted',/,9X,'depth',8X,
     # 'Area',8X,'Conveyance',6X,'width',6X,'perimeter',/,11X,'(ft)',
     #       7X,'(sq.ft)',8X,'(cfs)',10X,'(ft)',8X,'(ft)')
  105 FORMAT (9X,F5.2,7X,F6.1,7X,F8.1,7X,F5.2,8X,F6.1)
  106 FORMAT (/,20X,'<<User supplied discharge coefficients>>')
  107 FORMAT (22X,'CB12 = ',F4.2,10X,'C46 = ',F4.2)
  108 FORMAT (22X,'For type123 flow',5X,'For type 5 flow',/,
     #        25X,'C    (h1-z)/D',8x,'C    (h1-z)/D')
  109 FORMAT (23X,F4.2,2X,F7.2,8X,F4.2,2X,F7.2)
  110 FORMAT (4X,'Pipe arch radii (inches):  ',F6.1,'(bottom)',F6.1,
     #        '(top)',F6.1,'(corner)')
  111 FORMAT (15X,'(r or w)/B  KR or KW  Ktheta  Kproj    n      Inlet')    
  119 FORMAT(' ',2X)
  204 FORMAT (/,9X,'Barrel',36X,'Top',8X,'Wetted',/,9X,'depth',8X,
     # 'Area',8X,'Conveyance',6X,'width',6X,'perimeter',/,11X,'(m)',
     #       8X,'(sq. m)',8X,'(cms)',10X,'(m)',9X,'(m)')
  205 FORMAT (8X,F6.3,6X,F7.2,6X,F9.2,6X,F6.3,7X,F7.2)
  210 FORMAT (8X,'Pipe arch radii (cm):  ',F6.1,'(bottom)',F6.1,'(top)',
     #        F6.1,'(corner)')
C
      SIC=SI
      NOI=TBSIZE
      TCR=INT(TC/10)
      PNO=PNO+1
      ISTART=1
C      IPAGE=NPAGE-20
      IPAGE=NPAGE-21
      IF(IPAGE.GT.NOI) IPAGE=NOI
      CALL VERSON(OTNIT,PNO)
      WRITE (OTNIT,100) HTITLE,VRTID
      IF (TCR.EQ.2.AND.CWIDTH.GT.CULD(TBSIZE)) THEN
C       hor. ellipse
        WRITE (OTNIT,101) CTYPE(6)
      ELSE IF (TCR.EQ.2.AND.CWIDTH.LT.CULD(TBSIZE)) THEN
C       vert. ellipse            
        WRITE(OTNIT,101) CTYPE(5)
      ELSE  
        WRITE (OTNIT,101) CTYPE(TCR)
      ENDIF
      IF(TCR.EQ.1) THEN
         WRITE (OTNIT,111) 
      ELSE
         WRITE (OTNIT,102)
      ENDIF
C     Determine the pipe code: 1 = concrete and >1 = metal
      PC = TC - 10*NINT((REAL(TC)/10.))
C     Set KPROJ to 1.0 for concrete pipes
      IF (PC.EQ.1) KPROJ=1.0
      WRITE (OTNIT,103) RND,KWR,KWING,KPROJ,NRUFF,INLET
      IF (TC.EQ.3) THEN
        IF(SI.EQ.0.OR.SI.EQ.3)THEN
          WRITE (OTNIT,110) ARAD(1),ARAD(2),ARAD(3)
        ELSE
          WRITE (OTNIT,210) inch_to_cm(ARAD(1)),
     #           inch_to_cm(ARAD(2)),inch_to_cm(ARAD(3))
        ENDIF
        IPAGE=NPAGE-22
        IF(IPAGE.GT.NOI) IPAGE=NOI
      ENDIF
      WRITE (OTNIT,106)
      WRITE (OTNIT,107) CB12,C46IN
      WRITE (OTNIT,108)
      WRITE (OTNIT,109) (CP(I),HP(I),C5(I),H5(I),I=1,4)
      IF(SI.EQ.0.OR.SI.EQ.3)THEN
         WRITE (OTNIT,104)
      ELSE
         WRITE(OTNIT,204)
      ENDIF
15    CONTINUE
      IF(SI.EQ.0.OR.SI.EQ.3)THEN
        DO 10, I=ISTART,IPAGE
        WRITE (OTNIT,105) CULD(I),CULA(I),CULK(I),CULT(I),
     #                    CULWP(I)
10      CONTINUE
      ELSE
        DO 20, I=ISTART,IPAGE
        WRITE (OTNIT,205) ft_to_m(CULD(I)),
     #    ft2_to_m2(CULA(I)),ft3_to_m3(CULK(I)),
     #    ft_to_m(CULT(I)),ft_to_m(CULWP(I))
20      CONTINUE
      ENDIF
      IF (IPAGE.LT.NOI) THEN
        ISTART=IPAGE+1
        IPAGE=(NPAGE-8)+ISTART-1
        IF(IPAGE.GT.NOI)IPAGE=NOI
        PNO=PNO+1
        CALL VERSON(OTNIT,PNO)
        WRITE(OTNIT,100) HTITLE,VRTID
        WRITE(OTNIT,104)
      ELSE
        IPAGE=0
      ENDIF
      IF(IPAGE.GT.0) GO TO 15
C
      RETURN
      END
C
C=====WCULTB bof==============================================================
C
C=====TAB3TYP bof=============================================================
C
C-----Purpose:
C       prints flow type codes for 3 parameter constraint tables.
C     Programmed by: JM Fulford
C     Date:
C     Modified by:
C     Last modified:
C
      SUBROUTINE TAB3TYP
     I                  (GSN,OTNIT)
C
C-----Arguments:
      INTEGER GSN,OTNIT
C
C-----Module data:
      INCLUDE 'CULFLW.INC'
C
C-----Local variables:
      INTEGER ISTRUCT,I,J,K
C
      IF(TFLW.EQ.65) THEN
        WRITE(*,*)'65 was specified on the *CF record',
     #            'You must specify 5 or 6 for constraint table',
     #            '- table not built-'
        RETURN
      ENDIF
C     CONSTRAINT TABLE OUTPUT, HEADER AND TABLE RANGES
      ISTRUCT=30
      WRITE(OTNIT,'(A3)')'TAB'
      WRITE(OTNIT,'(I8,I2,1X,I2,1X,I2)')GSN,ISTRUCT,NTW,NQ
      WRITE(OTNIT,'(F9.3)') BASEL
      WRITE(OTNIT,'(12F7.2)')(H4TW(I),I=1,NTW)
      WRITE(OTNIT,'(10F8.0)')(Q(I),I=1,NQ)
C
      DO 10 J=1,NQ
        K=(J-1)*NTW
        WRITE(OTNIT,'(11I7)')(TYPE(K+I),I=1,NTW)
 10   CONTINUE
C
      RETURN
      END
C
C=====TAB3TYP eof==============================================================
C
C=====TAB3PAR bof=============================================================
C
C-----Purpose:
C       prints 3 parameter constraint tables. all elevations are
C       referenced to the base elevation BASEL at the outlet invert
C       of the culvert.
C     Programmed by: JM Fulford
C     Date:
C     Modified by:
C     Last modified:
C
      SUBROUTINE TAB3PAR
     I                  (GSN,OTNIT)
C
C-----Arguments:
      INTEGER GSN,OTNIT
C
C-----Module data:
      INCLUDE 'CULFLW.INC'
C
C-----Local variables:
      INTEGER ISTRUCT,INM,I
      REAL H(MAXTW)
C
      IF(TFLW.EQ.65) THEN
        WRITE(*,*)'65 was specified on the *CF record',
     #            'You must specify 5 or 6 for constraint table',
     #            '- table not built-'
        RETURN
      ENDIF
C     CONSTRAINT TABLE OUTPUT, HEADER AND TABLE RANGES
      ISTRUCT=30
      WRITE(OTNIT,'(A3)')'TAB'
      WRITE(OTNIT,'(I8,I2,1X,I2,1X,I2)')GSN,ISTRUCT,NTW,NQ
      WRITE(OTNIT,'(F9.3)') BASEL
      WRITE(OTNIT,'(12F7.2)')(H4TW(I),I=1,NTW)
      WRITE(OTNIT,'(10F8.0)')(Q(I),I=1,NQ)
C
      INM=1
 15   CONTINUE
      DO 10 I=1,NTW
        IF(H1E(INM).GT.-1.0) THEN
          H(I)=H1E(INM)-BASEL
        ELSE
          H(I)=H1E(INM)
        ENDIF
        IF((H4TW(I)+BASEL).GE.H4E(INM)) THEN
          INM=INM+1
        ENDIF
 10   CONTINUE
      WRITE(OTNIT,'(11F7.2)')(H(I),I=1,NTW)
      IF(INM.LT.INUM) GO TO 15
C
      RETURN
      END
C
C=====TAB3PAR eof==============================================================
C
C=====CHKD3 bof==============================================================
C-----Purpose:
C       Computes the energy equation between the inlet and outlet of
C       the culvert.  Used to determine the depth at the culvert inlet
C       for type 3 flow.
C       Equation used is in TWRI Chapter A3, "Measurements of Peak
C       Discharge at Culverts by Indirect Methods",pg 30.
C     Programmed by: JM Fulford
C     Date:
C     Modified by:
C     Last modified:
C
      REAL FUNCTION CHKD3
     I                (D3CHK,ERR)
C
C-----Arguments:
      INTEGER ERR
      REAL D3CHK
C
C-----Argument definitions:
C
C-----Module data:
      INCLUDE 'FCNCN2.INC'
      INCLUDE 'CULVRT.INC'
C
C-----Local variables:
      REAL A2,K2,T0,A3,K3,VGD3CK,H23D3K
C
C-----Externals:
      EXTERNAL CULINT
C
      ERR=0
      CALL CULINT(D3CHK,A3,K3,T0,ERR)
      VGD3CK=QG/(A3*A3)
      H23D3K=CLEN*CQ*CQ/K3
      CALL CULINT (DC,A2,K2,T0,ERR)
      IF(ERR.EQ.0)THEN
      CHKD3 = D3CHK +VGD3CK +H23D3K/K2  -(DC +(QG/(A2*A2)) +Z)
      ELSE
      ENDIF
C
      RETURN
      END
C
C=====CHKD3 eof==============================================================
