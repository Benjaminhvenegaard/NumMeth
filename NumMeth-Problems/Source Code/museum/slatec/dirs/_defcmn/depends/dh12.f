      SUBROUTINE DH12 (MODE, LPIVOT, L1, M, U, IUE, UP, C, ICE, ICV,
     +   NCV)
      INTEGER I, I2, I3, I4, ICE, ICV, INCR, IUE, J, KL1, KL2, KLP,
     *     L1, L1M1, LPIVOT, M, MML1P2, MODE, NCV
      DOUBLE PRECISION B, C, CL, CLINV, ONE, UL1M1, SM, U, UP, DDOT
      DIMENSION U(IUE,*), C(*)
C     BEGIN BLOCK PERMITTING ...EXITS TO 140
C***FIRST EXECUTABLE STATEMENT  DH12
         ONE = 1.0D0
C
C     ...EXIT
         IF (0 .GE. LPIVOT .OR. LPIVOT .GE. L1 .OR. L1 .GT. M) GO TO 140
         CL = ABS(U(1,LPIVOT))
         IF (MODE .EQ. 2) GO TO 40
C           ****** CONSTRUCT THE TRANSFORMATION. ******
            DO 10 J = L1, M
               CL = MAX(ABS(U(1,J)),CL)
   10       CONTINUE
            IF (CL .GT. 0.0D0) GO TO 20
C     .........EXIT
               GO TO 140
   20       CONTINUE
            CLINV = ONE/CL
            SM = (U(1,LPIVOT)*CLINV)**2
            DO 30 J = L1, M
               SM = SM + (U(1,J)*CLINV)**2
   30       CONTINUE
            CL = CL*SQRT(SM)
            IF (U(1,LPIVOT) .GT. 0.0D0) CL = -CL
            UP = U(1,LPIVOT) - CL
            U(1,LPIVOT) = CL
         GO TO 50
   40    CONTINUE
C        ****** APPLY THE TRANSFORMATION  I+U*(U**T)/B  TO C. ******
C
         IF (CL .GT. 0.0D0) GO TO 50
C     ......EXIT
            GO TO 140
   50    CONTINUE
C     ...EXIT
         IF (NCV .LE. 0) GO TO 140
         B = UP*U(1,LPIVOT)
C        B  MUST BE NONPOSITIVE HERE.  IF B = 0., RETURN.
C
         IF (B .LT. 0.0D0) GO TO 60
C     ......EXIT
            GO TO 140
   60    CONTINUE
         B = ONE/B
         MML1P2 = M - L1 + 2
         IF (MML1P2 .LE. 20) GO TO 80
            L1M1 = L1 - 1
            KL1 = 1 + (L1M1 - 1)*ICE
            KL2 = KL1
            KLP = 1 + (LPIVOT - 1)*ICE
            UL1M1 = U(1,L1M1)
            U(1,L1M1) = UP
            IF (LPIVOT .NE. L1M1) CALL DSWAP(NCV,C(KL1),ICV,C(KLP),ICV)
            DO 70 J = 1, NCV
               SM = DDOT(MML1P2,U(1,L1M1),IUE,C(KL1),ICE)
               SM = SM*B
               CALL DAXPY(MML1P2,SM,U(1,L1M1),IUE,C(KL1),ICE)
               KL1 = KL1 + ICV
   70       CONTINUE
            U(1,L1M1) = UL1M1
C     ......EXIT
            IF (LPIVOT .EQ. L1M1) GO TO 140
            KL1 = KL2
            CALL DSWAP(NCV,C(KL1),ICV,C(KLP),ICV)
         GO TO 130
   80    CONTINUE
            I2 = 1 - ICV + ICE*(LPIVOT - 1)
            INCR = ICE*(L1 - LPIVOT)
            DO 120 J = 1, NCV
               I2 = I2 + ICV
               I3 = I2 + INCR
               I4 = I3
               SM = C(I2)*UP
               DO 90 I = L1, M
                  SM = SM + C(I3)*U(1,I)
                  I3 = I3 + ICE
   90          CONTINUE
               IF (SM .EQ. 0.0D0) GO TO 110
                  SM = SM*B
                  C(I2) = C(I2) + SM*UP
                  DO 100 I = L1, M
                     C(I4) = C(I4) + SM*U(1,I)
                     I4 = I4 + ICE
  100             CONTINUE
  110          CONTINUE
  120       CONTINUE
  130    CONTINUE
  140 CONTINUE
      RETURN
      END
