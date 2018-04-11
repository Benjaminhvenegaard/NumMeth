      SUBROUTINE RPZERO (N, A, R, T, IFLG, S)
C
      COMPLEX R(*), T(*)
      REAL A(*), S(*)
C***FIRST EXECUTABLE STATEMENT  RPZERO
      N1=N+1
      DO 1 I=1,N1
      T(I)= CMPLX(A(I),0.0)
    1 CONTINUE
      CALL CPZERO(N,T,R,T(N+2),IFLG,S)
      RETURN
      END
