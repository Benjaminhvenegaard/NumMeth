      SUBROUTINE XERBLA (SRNAME, INFO)
C
C     ..    Scalar Arguments ..
      INTEGER            INFO
      CHARACTER*6        SRNAME
      CHARACTER*2        XERN1
C
C***FIRST EXECUTABLE STATEMENT  XERBLA
C
      WRITE (XERN1, '(I2)') INFO
      CALL XERMSG ('SLATEC', SRNAME, 'On entry to '//SRNAME//
     $             ' parameter number '//XERN1//' had an illegal value',
     $             INFO,1)
C
      RETURN
C
C     End of XERBLA.
C
      END
