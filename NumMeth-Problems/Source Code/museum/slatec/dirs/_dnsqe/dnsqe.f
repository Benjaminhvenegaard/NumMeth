      SUBROUTINE DNSQE (FCN, JAC, IOPT, N, X, FVEC, TOL, NPRINT, INFO,
     +   WA, LWA)
      INTEGER INDEX, INFO, IOPT, J, LR, LWA, MAXFEV, ML, MODE, MU, N,
     1     NFEV, NJEV, NPRINT
      DOUBLE PRECISION EPSFCN, FACTOR, FVEC(*), ONE, TOL, WA(*),
     1     X(*), XTOL, ZERO
      EXTERNAL FCN, JAC
      SAVE FACTOR, ONE, ZERO
      DATA FACTOR,ONE,ZERO /1.0D2,1.0D0,0.0D0/
C     BEGIN BLOCK PERMITTING ...EXITS TO 20
C***FIRST EXECUTABLE STATEMENT  DNSQE
         INFO = 0
C
C        CHECK THE INPUT PARAMETERS FOR ERRORS.
C
C     ...EXIT
         IF (IOPT .LT. 1 .OR. IOPT .GT. 2 .OR. N .LE. 0
     1       .OR. TOL .LT. ZERO .OR. LWA .LT. (3*N**2 + 13*N)/2)
     2      GO TO 20
C
C        CALL DNSQ.
C
         MAXFEV = 100*(N + 1)
         IF (IOPT .EQ. 2) MAXFEV = 2*MAXFEV
         XTOL = TOL
         ML = N - 1
         MU = N - 1
         EPSFCN = ZERO
         MODE = 2
         DO 10 J = 1, N
            WA(J) = ONE
   10    CONTINUE
         LR = (N*(N + 1))/2
         INDEX = 6*N + LR
         CALL DNSQ(FCN,JAC,IOPT,N,X,FVEC,WA(INDEX+1),N,XTOL,MAXFEV,ML,
     1             MU,EPSFCN,WA(1),MODE,FACTOR,NPRINT,INFO,NFEV,NJEV,
     2             WA(6*N+1),LR,WA(N+1),WA(2*N+1),WA(3*N+1),WA(4*N+1),
     3             WA(5*N+1))
         IF (INFO .EQ. 5) INFO = 4
   20 CONTINUE
      IF (INFO .EQ. 0) CALL XERMSG ('SLATEC', 'DNSQE',
     +   'INVALID INPUT PARAMETER.', 2, 1)
      RETURN
C
C     LAST CARD OF SUBROUTINE DNSQE.
C
      END
