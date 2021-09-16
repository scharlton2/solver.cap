C=====FCNCN2.INC bof=========================================================
C
C-----Common block
      INTEGER ISTEPS
      REAL TOL
      REAL VGDC,DCPZ,DCMZ,H12DC,H23DC,DC,D3,VGD3,H23D3,H12D2,
     #     QG,Z,ADC,AD3,CC,CQ,H23DCK,VGD2
      COMMON /FCNCON/  VGDC,DCPZ,DCMZ,H12DC,H23DC,DC,D3,VGD3,H23D3,
     #                 H12D2,QG,Z,ADC,AD3,CC,CQ,H23DCK,VGD2,TOL,ISTEPS
      SAVE /FCNCON/
C
C-----Variable definitions
C     VGDC   - Q*Q/(2g*Acc*Acc), velocity head for Dc
C     DCPZ   - Dc+Z
C     DCMZ   - Dc-Z
C     H12DC  - Lw*Q*Q/Kcc, head loss between sections 1 & 2.  Critical depth
C              at section 2.
C     H23DC  - L*Q*Q/Kcc*Kci, head loss between sections 2 & 3. Critical depth
C              at section 3.
C     DC     - Dc, culvert critical depth
C     D3     - depth at culvert outlet, assumed to be tailwater elevation.
C     VGD3   - Q*Q/(2g*Aco*Aco), velocity head at outlet for D3.
C     H23D3  - L*Q*Q/(Kco*Kci), head loss between sections 2 & 3. Depth at
C              outlet(section 3) assumed to be tailwater elevation.
C     H12D2  - Lw*Q*Q/(2g*Kci), head loss between sections 1 & 2. Depth at
C              inlet computed from function INLETD3 or INLETD2.
C     H23DC  - L*Q*Q/(Kcc*Kci), head loss between section 2 & 3. Depth at
C              outlet assumed critical.
C     QG     - Q*Q/2g
C     Z      - culvert drop.
C     ADC    - culvert area at Dc
C     AD3    - culvert area at D3
C     CC     - culvert coefficient
C              where:  Q-discharge, g-gravitational acceleration
C              Acc-culvert area at critical depth, Kcc-culvert conveyance for
C              critical depth, Kci-culvert conveyance at inlet, Kco-culvert
C              conveyance at outlet, L-barrel length, Lw-distance from approach
C              section to culvert inlet.
C     CQ     - discharge
C     TOL    - tolerance for root solver closure
C     ISTEPS - max. number of iterations for root solver
C=====FCNCN2.INC eof=========================================================
