      SUBROUTINE DROTG (DA, DB, DC, DS)
      DOUBLE PRECISION  DA, DB, DC, DS, U, V, R
C***FIRST EXECUTABLE STATEMENT  DROTG
      IF (ABS(DA) .LE. ABS(DB)) GO TO 10
C
C *** HERE ABS(DA) .GT. ABS(DB) ***
C
      U = DA + DA
      V = DB / U
C
C     NOTE THAT U AND R HAVE THE SIGN OF DA
C
      R = SQRT(0.25D0 + V**2) * U
C
C     NOTE THAT DC IS POSITIVE
C
      DC = DA / R
      DS = V * (DC + DC)
      DB = DS
      DA = R
      RETURN
C
C *** HERE ABS(DA) .LE. ABS(DB) ***
C
   10 IF (DB .EQ. 0.0D0) GO TO 20
      U = DB + DB
      V = DA / U
C
C     NOTE THAT U AND R HAVE THE SIGN OF DB
C     (R IS IMMEDIATELY STORED IN DA)
C
      DA = SQRT(0.25D0 + V**2) * U
C
C     NOTE THAT DS IS POSITIVE
C
      DS = DB / DA
      DC = V * (DS + DS)
      IF (DC .EQ. 0.0D0) GO TO 15
      DB = 1.0D0 / DC
      RETURN
   15 DB = 1.0D0
      RETURN
C
C *** HERE DA = DB = 0.0 ***
C
   20 DC = 1.0D0
      DS = 0.0D0
      RETURN
C
      END
