C=====ELLIPSE BOF===============================================
C
C-----Purpose:
C       Computes geometric properties for elliptical culverts
C     Programmed by:  J.M. Fulford
C     Date:  10.14.1998
C
      SUBROUTINE ELLIPSE
     I                  (RISE,SPAN,DEPTH,N,AREA,KONVEY,TW,WP)
C
C     
      REAL RISE,SPAN,DEPTH,N,KONVEY,TW,WP
C
      REAL A,B,W,HR,RATIO
C
      W= DEPTH - (RISE/2.0)
C      IF(RISE.GE.SPAN) THEN
C-----vertical ellipse
        A=SPAN*0.5
        B=RISE*0.5
C     ELSE
C-----horizontal ellipse
C        A=RISE*0.5
C        B=SPAN*0.5
C      ENDIF
      IF(ABS(DEPTH-RISE).LE.0.000001) THEN
        AREA = 3.14159*A*B
        WP = 6.28318*SQRT((A*A + B*B)*0.5)
        TW = 0.0
        HR= AREA/WP
      ELSE
        AREA = A*W*SQRT(B*B-W*W)/B + A*B*ASIN(W/B) + A*B*3.14159*0.5
        TW = A* SQRT(B*B - W*W)/B
        RATIO = -1.0*W/SQRT(W*W + TW*TW)
        TW=2.0*TW
        WP = 2.0 * SQRT((A*A + B*B)/2.0)*ACOS(RATIO)
        HR=AREA/WP
      ENDIF
      KONVEY=1.49*HR**(2.0/3.0)*AREA/N
C
      RETURN
      END
C
C=====ELLIPSE eOF===============================================
