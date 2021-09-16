C=====QTYP12.INC bof==========================================================
C
C-----Common block
      INTEGER ISTEPS
      REAL VHD,LK,LWLVH,HQ1,A3TW,A1,ATW,TOL
      COMMON /QTYP12/ VHD,LK,LWLVH,HQ1,A3TW,A1,ATW,TOL,ISTEPS
C
      SAVE /QTYP12/
C
C-----Variable definitions
C     VHD     - aph1/(2g*A1*A1), aph1 is the velocity coefficient,
C               g is gravity, A1 is area of approach section at H1
C               the approach depth
C     LK      - either Lw/K1 (DCTYP1); L*g (H2TYP2); L*g/K2 (DCTYP2)
C     LWLVH   - g*Lw/(K2*K1)-VHD (DCTYP2)
C     HQ1     - either Dc-Z (H2TYP2); H1 (DCTYP2); H1-Z (DCTYP1)
C     A3TW    - A**3/topwidth in culvert section at critical flow.
C     ATW     - A/topwidth in culvert section at critical flow
C     A1      - approach section area at h1
C
C=====QTYP12.INC eof==========================================================
