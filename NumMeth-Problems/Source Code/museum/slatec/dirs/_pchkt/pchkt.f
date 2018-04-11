      SUBROUTINE PCHKT (N, X, KNOTYP, T)
C
C*Internal Notes:
C
C  Since this is subsidiary to PCHBS, which validates its input before
C  calling, it is unnecessary for such validation to be done here.
C
C**End
C
C  Declare arguments.
C
      INTEGER  N, KNOTYP
      REAL  X(*), T(*)
C
C  Declare local variables.
C
      INTEGER  J, K, NDIM
      REAL  HBEG, HEND
C***FIRST EXECUTABLE STATEMENT  PCHKT
C
C  Initialize.
C
      NDIM = 2*N
C
C  Set interior knots.
C
      J = 1
      DO 20  K = 1, N
         J = J + 2
         T(J) = X(K)
         T(J+1) = T(J)
   20 CONTINUE
C     Assertion:  At this point T(3),...,T(NDIM+2) have been set and
C                 J=NDIM+1.
C
C  Set end knots according to KNOTYP.
C
      HBEG = X(2) - X(1)
      HEND = X(N) - X(N-1)
      IF (KNOTYP.EQ.1 )  THEN
C          Extrapolate.
         T(2) = X(1) - HBEG
         T(NDIM+3) = X(N) + HEND
      ELSE IF ( KNOTYP.EQ.2 )  THEN
C          Periodic.
         T(2) = X(1) - HEND
         T(NDIM+3) = X(N) + HBEG
      ELSE
C          Quadruple end knots.
         T(2) = X(1)
         T(NDIM+3) = X(N)
      ENDIF
      T(1) = T(2)
      T(NDIM+4) = T(NDIM+3)
C
C  Terminate.
C
      RETURN
C------------- LAST LINE OF PCHKT FOLLOWS ------------------------------
      END
