C=====CULVRT.INC bof========================================================
C
C     modified Dec 28, 2011 to increase CULNAM from 80 to 132 chars
C-----Parameters
      INTEGER TBSIZE, MXCOEF,MXCV
      PARAMETER (TBSIZE=31, MXCOEF=4, MXCV=50)
C
C-----Common block
      INTEGER TC,NHP,NH5,CULNIT,INLET,SI
      REAL CULD(TBSIZE),CULA(TBSIZE),CULK(TBSIZE),CULT(TBSIZE),THETA
     #    ,CULWP(TBSIZE),C5(MXCOEF),H5(MXCOEF),CP(MXCOEF),HP(MXCOEF)
     #    ,CB12,C46,KWR,RND,KWING,NRUFF,CLSRD,KPROJ,CLEN,ARAD(3),CWIDTH
      CHARACTER*16 VRTID
      CHARACTER*132 CULNAM
      COMMON / CULVRT / TC,CULNIT,CULD,CULA,CULK,CULT,C5,H5,CP,HP,
     #       CB12,C46,VRTID,KWR,RND,KWING,CULWP,NRUFF,NHP,NH5,CLSRD,CLEN
     #       ,INLET,KPROJ,CULNAM,ARAD,CWIDTH,SI,THETA
      SAVE /CULVRT/
C
C
C-----Variable definitions
C     TC     - culvert code, 1-box 2-pipe 3-pipe arch 4-nonstandard
C     CULNIT - file unit no. for culvert sections
C     CULD   - culvert depth for culvert properties at same index
C     CULA   - culvert area at culd
C     CULK   - culvert conveyance at CULD
C     CULT   - culvert top width at CULD
C     TOPW   - top width
C     C5     - culvert discharge coefficient for flow types 5
C     H5     - culvert depth for C5 at same index
C     CP     - culvert discharge coefficient for flow type 3
C     HP     - culvert depth for CP at same index
C     CB12   - culvert discharge coefficient for flow types 1-3
C     C46    - culvert discharge coefficient for flow type 4-6
C     KWR     -entrance rounding or beveling adjustment to discharge coefficient
C             (pg 39 TWRI & pg 40 TWRI)
C     KWING  -wingwall adjustment to discharge coefficient (pg 42 TWRI)
C     INLET  -inlet code for program where:
C             1-vertical headwall flush
C             2-mitered
C             3-bellmouth or tongue & groove concrete pipe (pg 39-40 TWRI)
C     KPROJ  - projecting pipe adjustment to discharge coefficient (pg 42)
C     CLSRD  - reference distance for culvert section
C     CLEN   - culvert length
C     ARAD   - pipe arch radii, ARAD(1)-bottom, ARAD(2)-top, ARAD(3)-corner
C     CWIDTH - culvert width, max station - min station or the b variable
C     THETA  - wingwall angle in degrees
C     RND    - rounding or beveling ratio 
C     CGNSID - cgns file handle
C
C=====CULVRT.INC eof=========================================================
