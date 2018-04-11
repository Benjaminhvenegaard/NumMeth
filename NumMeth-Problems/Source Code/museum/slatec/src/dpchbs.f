      SUBROUTINE DPCHBS (N, X, F, D, INCFD, KNOTYP, NKNOTS, T, BCOEF,
     +   NDIM, KORD, IERR)
C
C*Internal Notes:
C
C**End
C
C  Declare arguments.
C
      INTEGER  N, INCFD, KNOTYP, NKNOTS, NDIM, KORD, IERR
      DOUBLE PRECISION  X(*), F(INCFD,*), D(INCFD,*), T(*), BCOEF(*)
C
C  Declare local variables.
C
      INTEGER  K, KK
      DOUBLE PRECISION  DOV3, HNEW, HOLD
      CHARACTER*8  LIBNAM, SUBNAM
C***FIRST EXECUTABLE STATEMENT  DPCHBS
C
C  Initialize.
C
      NDIM = 2*N
      KORD = 4
      IERR = 0
      LIBNAM = 'SLATEC'
      SUBNAM = 'DPCHBS'
C
C  Check argument validity.  Set up knot sequence if OK.
C
      IF ( KNOTYP.GT.2 )  THEN
         IERR = -1
         CALL XERMSG (LIBNAM, SUBNAM, 'KNOTYP GREATER THAN 2', IERR, 1)
         RETURN
      ENDIF
      IF ( KNOTYP.LT.0 )  THEN
         IF ( NKNOTS.NE.NDIM+4 )  THEN
            IERR = -2
            CALL XERMSG (LIBNAM, SUBNAM,
     *                    'KNOTYP.LT.0 AND NKNOTS.NE.(2*N+4)', IERR, 1)
            RETURN
         ENDIF
      ELSE
C          Set up knot sequence.
         NKNOTS = NDIM + 4
         CALL DPCHKT (N, X, KNOTYP, T)
      ENDIF
C
C  Compute B-spline coefficients.
C
      HNEW = T(3) - T(1)
      DO 40  K = 1, N
         KK = 2*K
         HOLD = HNEW
C          The following requires mixed mode arithmetic.
         DOV3 = D(1,K)/3
         BCOEF(KK-1) = F(1,K) - HOLD*DOV3
C          The following assumes T(2*K+1) = X(K).
         HNEW = T(KK+3) - T(KK+1)
         BCOEF(KK) = F(1,K) + HNEW*DOV3
   40 CONTINUE
C
C  Terminate.
C
      RETURN
C------------- LAST LINE OF DPCHBS FOLLOWS -----------------------------
      END
