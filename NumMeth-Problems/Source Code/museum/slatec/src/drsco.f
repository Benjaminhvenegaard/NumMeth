      SUBROUTINE DRSCO (RSAV, ISAV)
C-----------------------------------------------------------------------
C THIS ROUTINE RESTORES FROM RSAV AND ISAV THE CONTENTS OF COMMON
C BLOCK DDEBD1  , WHICH IS USED INTERNALLY IN THE DDEBDF
C PACKAGE.  THIS PRESUMES THAT RSAV AND ISAV WERE LOADED BY MEANS
C OF SUBROUTINE DSVCO OR THE EQUIVALENT.
C-----------------------------------------------------------------------
C
      INTEGER I, ILS, ISAV, LENILS, LENRLS
      DOUBLE PRECISION RLS, RSAV
      DIMENSION RSAV(*),ISAV(*)
      SAVE LENRLS, LENILS
      COMMON /DDEBD1/ RLS(218),ILS(33)
      DATA LENRLS /218/, LENILS /33/
C
C***FIRST EXECUTABLE STATEMENT  DRSCO
      DO 10 I = 1, LENRLS
         RLS(I) = RSAV(I)
   10 CONTINUE
      DO 20 I = 1, LENILS
         ILS(I) = ISAV(I)
   20 CONTINUE
      RETURN
C     ----------------------- END OF SUBROUTINE DRSCO
C     -----------------------
      END
