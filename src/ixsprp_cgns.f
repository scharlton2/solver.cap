C-----Purpose:
C       initialize culvert properties table
C     Programmed by: Keisuke Inoue

      LOGICAL FUNCTION IXSPRP_CGNS(XSID,BASEL,SI)
      IMPLICIT NONE
    
C-----Arguments:
      CHARACTER*16, INTENT(IN) :: XSID(1)
      REAL, INTENT(IN) :: BASEL
      INTEGER, INTENT(IN) :: SI

C-----Argument definitions:
C     XSID   - character identifier for approach section
C     BASEL  - Datum elevation

C-----Module data:
      INCLUDE 'APPROC.INC'

C-----Variables:
      INTEGER FLGS(1),I

C     ONLY FOR DEVELOPMENT
      XSNAM='mercer.dat'
      
      IXSPRP_CGNS=.FALSE.
      FLGS(1)=1
      I=0
      CALL HYDIE_CGNS(SI)
      SIA=SI
      IF(FLGS(1).EQ.1.AND.APPID.EQ.XSID(1)) THEN
        DO I=1,NQW
          W1(I)=W1(I)-BASEL
        END DO
        IXSPRP_CGNS=.TRUE.
      ENDIF
C
      RETURN

      END FUNCTION
