C   Contains four subroutines for converting from ft-s to m-s and
C   m-s to ft-s for discharge and length
C=====MTRFT bof============================================================
C
C-----Purpose:
C       convert length from meter to feet
C     Programmed by: JM Fulford
C     Date:  10.04.96
C     Modified by:
C     Last modified:
C
      SUBROUTINE MTRFT
     I                (SIZE, NUMBERS)
C
C-----Arguments:
      INTEGER SIZE
      REAL NUMBERS(SIZE)
C
C-----Argument definitions:
C     SIZE    - size of array of reals to convert from meters to feet
C     NUMBERS - real array of numbers to convert
C
C-----Local Variables
      INTEGER I
C
      DO 1 I=1,SIZE
        NUMBERS(I)=NUMBERS(I)*(100./30.48)
 1    CONTINUE
C
      RETURN
      END
C
C=====MTRFT eof===============================================================
C=====CMBFT bof============================================================
C
C-----Purpose:
C       convert length from meter to feet
C     Programmed by: JM Fulford
C     Date:  10.04.96
C     Modified by:
C     Last modified:
C
      SUBROUTINE CMFT
     I                (SIZE, NUMBERS)
C
C-----Arguments:
      INTEGER SIZE
      REAL NUMBERS(SIZE)
C
C-----Argument definitions:
C     SIZE    - size of array of reals to convert from meters to feet
C     NUMBERS - real array of numbers to convert
C
C-----Local Variables
      INTEGER I
C
      DO 1 I=1,SIZE
        NUMBERS(I)=NUMBERS(I)/30.48
 1    CONTINUE
C
      RETURN
      END
C
C=====MTRFT eof===============================================================
C=====FTMTR bof============================================================
C
C-----Purpose:
C       convert length from feet to meter
C     Programmed by: JM Fulford
C     Date:  10.04.96
C     Modified by:
C     Last modified:
C
      REAL FUNCTION FTMTR
     I                (NUMBER)
C
C-----Arguments:
      REAL NUMBER
C
C-----Argument definitions:
C     NUMBER - real number to convert
C
C-----Local Variables
      FTMTR=NUMBER*(30.48/100.)
C
      RETURN
      END
C
C=====FTMTR eof===============================================================
C
C
C=====FT2M2 bof============================================================
C
C-----Purpose:
C       convert discharge from feet squared to meter squared
C     Programmed by: JM Fulford
C     Date:  10.04.96
C     Modified by:
C     Last modified:
C
      REAL FUNCTION FT2M2
     I                (NUMBER)
C
C-----Arguments:
      REAL NUMBER
C
C-----Argument definitions:
C     NUMBER - real number to convert
C
C-----Local Variables
      FT2M2=NUMBER*(30.48/100.)*(30.48/100.)
C
      RETURN
      END
C
C=====FT2M2 eof===============================================================
C
C=====M3FT3 bof===============================================================
C-----Purpose:
C       convert discharge from meter squared to feet squared
C     Programmed by: JM Fulford
C     Date:  10.04.96
C     Modified by:
C     Last modified:
C
      SUBROUTINE M3FT3
     I                (SIZE, NUMBERS)
C
C-----Arguments:
      INTEGER SIZE
      REAL NUMBERS(SIZE)
C
C-----Argument definitions:
C     SIZE    - size of array of reals to convert from feet to meters
C     NUMBERS - real array of numbers to convert
C
C-----Local Variables
      INTEGER I
C
      DO 1 I=1,SIZE
        NUMBERS(I)=NUMBERS(I)*(100./30.48)*(100./30.48)*(100./30.48)
 1    CONTINUE
C
      RETURN
      END
C
C=====M2FT2 eof===============================================================
C
C=====FT3M3 bof============================================================
C
C-----Purpose:
C       convert discharge from feet squared to meter squared
C     Programmed by: JM Fulford
C     Date:  10.04.96
C     Modified by:
C     Last modified:
C
      REAL FUNCTION FT3M3
     I                (NUMBER)
C
C-----Arguments:
      REAL NUMBER
C
C-----Argument definitions:
C     NUMBER - real number to convert
C
C-----Local Variables
      FT3M3=NUMBER*(30.48/100.)*(30.48/100.)*(30.48/100.)
C
      RETURN
      END
C
C=====FT3M3 eof===============================================================
