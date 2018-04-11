      SUBROUTINE DOHTRL (Q, N, NRDA, DIAG, IRANK, DIV, TD)
      DOUBLE PRECISION DDOT
      INTEGER IRANK, IRP, J, K, KIR, KIRM, L, N, NMIR, NRDA
      DOUBLE PRECISION DD, DIAG(*), DIAGK, DIV(*), Q(NRDA,*), QS, SIG,
     1     SQD, TD(*), TDV
C***FIRST EXECUTABLE STATEMENT  DOHTRL
      NMIR = N - IRANK
      IRP = IRANK + 1
      DO 40 K = 1, IRANK
         KIR = IRP - K
         DIAGK = DIAG(KIR)
         SIG = (DIAGK*DIAGK) + DDOT(NMIR,Q(IRP,KIR),1,Q(IRP,KIR),1)
         DD = SIGN(SQRT(SIG),-DIAGK)
         DIV(KIR) = DD
         TDV = DIAGK - DD
         TD(KIR) = TDV
         IF (K .EQ. IRANK) GO TO 30
            KIRM = KIR - 1
            SQD = DD*DIAGK - SIG
            DO 20 J = 1, KIRM
               QS = ((TDV*Q(KIR,J))
     1               + DDOT(NMIR,Q(IRP,J),1,Q(IRP,KIR),1))/SQD
               Q(KIR,J) = Q(KIR,J) + QS*TDV
               DO 10 L = IRP, N
                  Q(L,J) = Q(L,J) + QS*Q(L,KIR)
   10          CONTINUE
   20       CONTINUE
   30    CONTINUE
   40 CONTINUE
      RETURN
      END
