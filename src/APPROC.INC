C=====APPROC.INC bof========================================================
C
C     modified Dec 28, 2011 to increase XSNAM from 80 to 132 chars
C-----Parameters
      INTEGER TAMAX
      PARAMETER (TAMAX=25)
C
C-----Common block
      INTEGER NQW,XSNIT,SIA
      REAL W1(TAMAX),A1(TAMAX),APH1(TAMAX),K1(TAMAX),B1(TAMAX),QC(TAMAX)
     #     ,APSRD,WP1(TAMAX)
      CHARACTER*16 APPID
      CHARACTER*132 XSNAM
      COMMON / APPROC / NQW,XSNIT,W1,A1,APH1,K1,B1,QC,APPID,APSRD,XSNAM
     #                  ,WP1,SIA
      SAVE /APPROC/
C
C-----Variable definitions
C     SIA    - unit flag input/output 0-fts/fts 1-ms/fts 2-fts/ms 3-ms/fts
C     TAMAX  - max no. of table elements
C     NQW    - no. of elevations for which table is computed
C     XSNIT  - unit number of flat file that data is on
C     W1     - water depth in approach section
C     A1     - area at W1
C     APH1   - alpha, the coefficient for adjustment of vel. head
C     K1     - conveyance at W1
C     B1     - top width at W1
C     WP1    - wetted perimeter
C     QC     - approximate discharge based on A*sqrt(gD) where D equals
C              A/B1
C     APSRD  - reference distance for approach section
C
C=====APPROC.INC eof=========================================================
