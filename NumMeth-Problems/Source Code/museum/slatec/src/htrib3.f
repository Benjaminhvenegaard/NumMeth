      SUBROUTINE HTRIB3 (NM, N, A, TAU, M, ZR, ZI)
C
      INTEGER I,J,K,L,M,N,NM
      REAL A(NM,*),TAU(2,*),ZR(NM,*),ZI(NM,*)
      REAL H,S,SI
C
C***FIRST EXECUTABLE STATEMENT  HTRIB3
      IF (M .EQ. 0) GO TO 200
C     .......... TRANSFORM THE EIGENVECTORS OF THE REAL SYMMETRIC
C                TRIDIAGONAL MATRIX TO THOSE OF THE HERMITIAN
C                TRIDIAGONAL MATRIX. ..........
      DO 50 K = 1, N
C
         DO 50 J = 1, M
            ZI(K,J) = -ZR(K,J) * TAU(2,K)
            ZR(K,J) = ZR(K,J) * TAU(1,K)
   50 CONTINUE
C
      IF (N .EQ. 1) GO TO 200
C     .......... RECOVER AND APPLY THE HOUSEHOLDER MATRICES ..........
      DO 140 I = 2, N
         L = I - 1
         H = A(I,I)
         IF (H .EQ. 0.0E0) GO TO 140
C
         DO 130 J = 1, M
            S = 0.0E0
            SI = 0.0E0
C
            DO 110 K = 1, L
               S = S + A(I,K) * ZR(K,J) - A(K,I) * ZI(K,J)
               SI = SI + A(I,K) * ZI(K,J) + A(K,I) * ZR(K,J)
  110       CONTINUE
C     .......... DOUBLE DIVISIONS AVOID POSSIBLE UNDERFLOW ..........
            S = (S / H) / H
            SI = (SI / H) / H
C
            DO 120 K = 1, L
               ZR(K,J) = ZR(K,J) - S * A(I,K) - SI * A(K,I)
               ZI(K,J) = ZI(K,J) - SI * A(I,K) + S * A(K,I)
  120       CONTINUE
C
  130    CONTINUE
C
  140 CONTINUE
C
  200 RETURN
      END
