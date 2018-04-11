      SUBROUTINE SHEQR (A, LDA, N, Q, INFO, IJOB)
C         The following is for optimized compilation on LLNL/LTSS Crays.
CLLL. OPTIMIZE
C     .. Scalar Arguments ..
      INTEGER IJOB, INFO, LDA, N
C     .. Array Arguments ..
      REAL A(LDA,*), Q(*)
C     .. Local Scalars ..
      REAL C, S, T, T1, T2
      INTEGER I, IQ, J, K, KM1, KP1, NM1
C     .. Intrinsic Functions ..
      INTRINSIC ABS, SQRT
C***FIRST EXECUTABLE STATEMENT  SHEQR
      IF (IJOB .GT. 1) GO TO 70
C   -------------------------------------------------------------------
C         A new factorization is desired.
C   -------------------------------------------------------------------
C         QR decomposition without pivoting.
C
      INFO = 0
      DO 60 K = 1, N
         KM1 = K - 1
         KP1 = K + 1
C
C           Compute K-th column of R.
C           First, multiply the K-th column of A by the previous
C           K-1 Givens rotations.
C
         IF (KM1 .LT. 1) GO TO 20
         DO 10 J = 1, KM1
            I = 2*(J-1) + 1
            T1 = A(J,K)
            T2 = A(J+1,K)
            C = Q(I)
            S = Q(I+1)
            A(J,K) = C*T1 - S*T2
            A(J+1,K) = S*T1 + C*T2
 10      CONTINUE
C
C         Compute Givens components C and S.
C
 20      CONTINUE
         IQ = 2*KM1 + 1
         T1 = A(K,K)
         T2 = A(KP1,K)
         IF( T2.EQ.0.0E0 ) THEN
            C = 1
            S = 0
         ELSEIF( ABS(T2).GE.ABS(T1) ) THEN
            T = T1/T2
            S = -1.0E0/SQRT(1.0E0+T*T)
            C = -S*T
         ELSE
            T = T2/T1
            C = 1.0E0/SQRT(1.0E0+T*T)
            S = -C*T
         ENDIF
         Q(IQ) = C
         Q(IQ+1) = S
         A(K,K) = C*T1 - S*T2
         IF( A(K,K).EQ.0.0E0 ) INFO = K
 60   CONTINUE
      RETURN
C   -------------------------------------------------------------------
C         The old factorization of a will be updated.  A row and a
C         column has been added to the matrix A.  N by N-1 is now
C         the old size of the matrix.
C   -------------------------------------------------------------------
 70   CONTINUE
      NM1 = N - 1
C   -------------------------------------------------------------------
C         Multiply the new column by the N previous Givens rotations.
C   -------------------------------------------------------------------
      DO 100 K = 1,NM1
         I = 2*(K-1) + 1
         T1 = A(K,N)
         T2 = A(K+1,N)
         C = Q(I)
         S = Q(I+1)
         A(K,N) = C*T1 - S*T2
         A(K+1,N) = S*T1 + C*T2
 100  CONTINUE
C   -------------------------------------------------------------------
C         Complete update of decomposition by forming last Givens
C         rotation, and multiplying it times the column
C         vector(A(N,N),A(NP1,N)).
C   -------------------------------------------------------------------
      INFO = 0
      T1 = A(N,N)
      T2 = A(N+1,N)
      IF ( T2.EQ.0.0E0 ) THEN
         C = 1
         S = 0
      ELSEIF( ABS(T2).GE.ABS(T1) ) THEN
         T = T1/T2
         S = -1.0E0/SQRT(1.0E0+T*T)
         C = -S*T
      ELSE
         T = T2/T1
         C = 1.0E0/SQRT(1.0E0+T*T)
         S = -C*T
      ENDIF
      IQ = 2*N - 1
      Q(IQ) = C
      Q(IQ+1) = S
      A(N,N) = C*T1 - S*T2
      IF (A(N,N) .EQ. 0.0E0) INFO = N
      RETURN
C------------- LAST LINE OF SHEQR FOLLOWS ----------------------------
      END
