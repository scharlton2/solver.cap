C=====CULFLW.INC bof========================================================
C
C-----Parameters
      INTEGER MAXNO,MAXQ,MAXTW
      PARAMETER (MAXQ=48,MAXTW=50)
      PARAMETER (MAXNO=MAXQ*MAXTW)
C
C-----Common block
      INTEGER INUM,NQ,NTW,TYPE(MAXNO),ERRCOD(MAXNO),PTR,TFLW
     #     ,CNTRS(MAXNO),H4TWID(MAXNO),SIC
      REAL Q(MAXQ),H1E(MAXNO),H4E(MAXNO),D2(MAXNO),D3A(MAXNO),
     #     PKON(MAXNO),H4TW(MAXTW),CULCRTD(MAXNO),D,L,BASEL,ZDROP,LW
     #     ,HFLW,Q1,Q2,Q5,Q6
      CHARACTER*16 SECID,CULID
      COMMON / CULFLW / INUM,TYPE,ERRCOD,Q,H1E,H4E,D2,D3A,PKON,SECID,
     #                  CULID,H4TW,D,L,BASEL,ZDROP,LW,PTR,NTW,NQ,TFLW,
     #                  HFLW,CNTRS,H4TWID,Q1,Q2,Q5,Q6,CULCRTD,SIC
      SAVE /CULFLW/ 
C
C-----Variable definitions
C     SIC     - units flag input/output 0-fts/fts 1-ms/ms 2-fts/ms 3-ms/fts
C     MAXNO  - max. no. of flow computations dimensioned
C     NQ     - no. of discharges used in culvert flow computations
C     MTW    - no. of tail water elevations used in culvert flow computations
C     INUM    - no. of flow computations made
C     TYPE   - type culvert flow computed(see TWRI by Bohaine)
C     ERRCOD - error code associated with computation
C     Q      - discharge
C     H1E    - approach section elevation
C     H4E    - culvert exit elevation
C     D2     - culvert entrance depth
C     D3     - culvert exit depth
C     PKON   - discharge coefficient used
C     SECID  - approach section id
C     CULID  - culvert id
C     PTR    - pointer to entry in common arrays, usually the currently in
C              use value
C     TFLW   - type flow for rapid inlet conditions, type 5, type 6 or both
C     HFLW   - approach depth at which culvert becomes hydraulically short
C              and type 6 flow occurs.
C     Q1     - discharge computed for 1.2*D+ZDROP, for type 1 flow
C     Q2     - discharge computed for 1.2*D+ZDROP, for type 2 flow
C     Q5     - discharge computed for 1.5*D+ZDROP, for type 5 flow
C     Q6     - discharge computed for 1.5*D+ZDROP, for type 6 flow
C     CULCRTD- critical flow depth in culvert barrel for discharge
C
C=====CULFLW eof=============================================================
