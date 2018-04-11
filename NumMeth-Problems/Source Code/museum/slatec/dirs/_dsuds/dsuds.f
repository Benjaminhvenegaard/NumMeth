      SUBROUTINE DSUDS (A, X, B, NEQ, NUK, NRDA, IFLAG, MLSO, WORK,
     +   IWORK)
      INTEGER IFLAG, IL, IP, IS, IWORK(*), KS, KT, KU, KV, MLSO, NEQ,
     1     NRDA, NUK
      DOUBLE PRECISION A(NRDA,*), B(*), WORK(*), X(*)
C
C***FIRST EXECUTABLE STATEMENT  DSUDS
      IS = 2
      IP = 3
      IL = IP + NEQ
      KV = 1 + NEQ
      KT = KV + NEQ
      KS = KT + NEQ
      KU = KS + NUK
C
      CALL DLSSUD(A,X,B,NEQ,NUK,NRDA,WORK(KU),NUK,IFLAG,MLSO,IWORK(1),
     1            IWORK(IS),A,WORK(1),IWORK(IP),B,WORK(KV),WORK(KT),
     2            IWORK(IL),WORK(KS))
C
      RETURN
      END
