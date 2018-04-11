      SUBROUTINE OHTROL (Q, N, NRDA, DIAG, IRANK, DIV, TD)
      DIMENSION Q(NRDA,*),DIAG(*),DIV(*),TD(*)
C***FIRST EXECUTABLE STATEMENT  OHTROL
      NMIR=N-IRANK
      IRP=IRANK+1
      DO 30 K=1,IRANK
         KIR=IRP-K
         DIAGK=DIAG(KIR)
         SIG=(DIAGK*DIAGK)+SDOT(NMIR,Q(IRP,KIR),1,Q(IRP,KIR),1)
         DD=SIGN(SQRT(SIG),-DIAGK)
         DIV(KIR)=DD
         TDV=DIAGK-DD
         TD(KIR)=TDV
         IF (K .EQ. IRANK) GO TO 30
         KIRM=KIR-1
         SQD=DD*DIAGK-SIG
         DO 20 J=1,KIRM
            QS=((TDV*Q(KIR,J))+SDOT(NMIR,Q(IRP,J),1,Q(IRP,KIR),1))
     1               /SQD
            Q(KIR,J)=Q(KIR,J)+QS*TDV
            DO 10 L=IRP,N
   10          Q(L,J)=Q(L,J)+QS*Q(L,KIR)
   20    CONTINUE
   30 CONTINUE
      RETURN
      END
