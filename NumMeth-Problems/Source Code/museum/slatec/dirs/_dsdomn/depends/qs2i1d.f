      SUBROUTINE QS2I1D (IA, JA, A, N, KFLAG)
CVD$R NOVECTOR
CVD$R NOCONCUR
C     .. Scalar Arguments ..
      INTEGER KFLAG, N
C     .. Array Arguments ..
      DOUBLE PRECISION A(N)
      INTEGER IA(N), JA(N)
C     .. Local Scalars ..
      DOUBLE PRECISION R, TA, TTA
      INTEGER I, IIT, IJ, IT, J, JJT, JT, K, KK, L, M, NN
C     .. Local Arrays ..
      INTEGER IL(21), IU(21)
C     .. External Subroutines ..
      EXTERNAL XERMSG
C     .. Intrinsic Functions ..
      INTRINSIC ABS, INT
C***FIRST EXECUTABLE STATEMENT  QS2I1D
      NN = N
      IF (NN.LT.1) THEN
         CALL XERMSG ('SLATEC', 'QS2I1D',
     $      'The number of values to be sorted was not positive.', 1, 1)
         RETURN
      ENDIF
      IF( N.EQ.1 ) RETURN
      KK = ABS(KFLAG)
      IF ( KK.NE.1 ) THEN
         CALL XERMSG ('SLATEC', 'QS2I1D',
     $      'The sort control parameter, K, was not 1 or -1.', 2, 1)
         RETURN
      ENDIF
C
C     Alter array IA to get decreasing order if needed.
C
      IF( KFLAG.LT.1 ) THEN
         DO 20 I=1,NN
            IA(I) = -IA(I)
 20      CONTINUE
      ENDIF
C
C     Sort IA and carry JA and A along.
C     And now...Just a little black magic...
      M = 1
      I = 1
      J = NN
      R = .375D0
 210  IF( R.LE.0.5898437D0 ) THEN
         R = R + 3.90625D-2
      ELSE
         R = R-.21875D0
      ENDIF
 225  K = I
C
C     Select a central element of the array and save it in location
C     it, jt, at.
C
      IJ = I + INT ((J-I)*R)
      IT = IA(IJ)
      JT = JA(IJ)
      TA = A(IJ)
C
C     If first element of array is greater than it, interchange with it.
C
      IF( IA(I).GT.IT ) THEN
         IA(IJ) = IA(I)
         IA(I)  = IT
         IT     = IA(IJ)
         JA(IJ) = JA(I)
         JA(I)  = JT
         JT     = JA(IJ)
         A(IJ)  = A(I)
         A(I)   = TA
         TA     = A(IJ)
      ENDIF
      L=J
C
C     If last element of array is less than it, swap with it.
C
      IF( IA(J).LT.IT ) THEN
         IA(IJ) = IA(J)
         IA(J)  = IT
         IT     = IA(IJ)
         JA(IJ) = JA(J)
         JA(J)  = JT
         JT     = JA(IJ)
         A(IJ)  = A(J)
         A(J)   = TA
         TA     = A(IJ)
C
C     If first element of array is greater than it, swap with it.
C
         IF ( IA(I).GT.IT ) THEN
            IA(IJ) = IA(I)
            IA(I)  = IT
            IT     = IA(IJ)
            JA(IJ) = JA(I)
            JA(I)  = JT
            JT     = JA(IJ)
            A(IJ)  = A(I)
            A(I)   = TA
            TA     = A(IJ)
         ENDIF
      ENDIF
C
C     Find an element in the second half of the array which is
C     smaller than it.
C
  240 L=L-1
      IF( IA(L).GT.IT ) GO TO 240
C
C     Find an element in the first half of the array which is
C     greater than it.
C
  245 K=K+1
      IF( IA(K).LT.IT ) GO TO 245
C
C     Interchange these elements.
C
      IF( K.LE.L ) THEN
         IIT   = IA(L)
         IA(L) = IA(K)
         IA(K) = IIT
         JJT   = JA(L)
         JA(L) = JA(K)
         JA(K) = JJT
         TTA   = A(L)
         A(L)  = A(K)
         A(K)  = TTA
         GOTO 240
      ENDIF
C
C     Save upper and lower subscripts of the array yet to be sorted.
C
      IF( L-I.GT.J-K ) THEN
         IL(M) = I
         IU(M) = L
         I = K
         M = M+1
      ELSE
         IL(M) = K
         IU(M) = J
         J = L
         M = M+1
      ENDIF
      GO TO 260
C
C     Begin again on another portion of the unsorted array.
C
  255 M = M-1
      IF( M.EQ.0 ) GO TO 300
      I = IL(M)
      J = IU(M)
  260 IF( J-I.GE.1 ) GO TO 225
      IF( I.EQ.J ) GO TO 255
      IF( I.EQ.1 ) GO TO 210
      I = I-1
  265 I = I+1
      IF( I.EQ.J ) GO TO 255
      IT = IA(I+1)
      JT = JA(I+1)
      TA =  A(I+1)
      IF( IA(I).LE.IT ) GO TO 265
      K=I
  270 IA(K+1) = IA(K)
      JA(K+1) = JA(K)
      A(K+1)  =  A(K)
      K = K-1
      IF( IT.LT.IA(K) ) GO TO 270
      IA(K+1) = IT
      JA(K+1) = JT
      A(K+1)  = TA
      GO TO 265
C
C     Clean up, if necessary.
C
  300 IF( KFLAG.LT.1 ) THEN
         DO 310 I=1,NN
            IA(I) = -IA(I)
 310     CONTINUE
      ENDIF
      RETURN
C------------- LAST LINE OF QS2I1D FOLLOWS ----------------------------
      END
