      SUBROUTINE CDPSC (KSGN, N, NQ, YH)
      INTEGER I, J, J1, J2, KSGN, N, NQ
      COMPLEX YH(N,*)
C***FIRST EXECUTABLE STATEMENT  CDPSC
      IF (KSGN .GT. 0) THEN
        DO 10 J1 = 1,NQ
          DO 10 J2 = J1,NQ
            J = NQ - J2 + J1
            DO 10 I = 1,N
 10           YH(I,J) = YH(I,J) + YH(I,J+1)
      ELSE
        DO 30 J1 = 1,NQ
          DO 30 J2 = J1,NQ
            J = NQ - J2 + J1
            DO 30 I = 1,N
 30           YH(I,J) = YH(I,J) - YH(I,J+1)
      END IF
      RETURN
      END
