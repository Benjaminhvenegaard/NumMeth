      DOUBLE PRECISION FUNCTION DQDOTA (N, DB, QC, DX, INCX, DY, INCY)
      DOUBLE PRECISION DX(*), DY(*), DB
      INTEGER  QC(30), QX(30), QY(30)
      COMMON /MPCOM/  MPB, MPT, MPM, MPLUN, MPMXR, MPR(30)
      SAVE I1
      DATA  I1 / 0 /
C***FIRST EXECUTABLE STATEMENT  DQDOTA
      IF (I1 .EQ. 0) CALL MPBLAS(I1)
      IF (DB .EQ. 0.D0) GO TO 20
      CALL MPCDM(DB, QX)
      CALL MPADD(QC, QX, QC)
   20 IF (N .EQ. 0) GO TO 40
      IX = 1
      IY = 1
      IF (INCX .LT. 0) IX = (-N + 1) * INCX + 1
      IF (INCY .LT. 0) IY = (-N + 1) * INCY + 1
      DO  30  I = 1,N
         CALL MPCDM(DX(IX), QX)
         CALL MPCDM(DY(IY), QY)
         CALL MPMUL(QX, QY, QX)
         CALL MPADD(QC, QX, QC)
         IX = IX + INCX
         IY = IY + INCY
   30 CONTINUE
   40 CALL MPCMD(QC, DQDOTA)
      RETURN
      END
