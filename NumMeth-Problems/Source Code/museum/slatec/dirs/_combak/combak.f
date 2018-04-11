      SUBROUTINE COMBAK (NM, LOW, IGH, AR, AI, INT, M, ZR, ZI)
C
      INTEGER I,J,M,LA,MM,MP,NM,IGH,KP1,LOW,MP1
      REAL AR(NM,*),AI(NM,*),ZR(NM,*),ZI(NM,*)
      REAL XR,XI
      INTEGER INT(*)
C
C***FIRST EXECUTABLE STATEMENT  COMBAK
      IF (M .EQ. 0) GO TO 200
      LA = IGH - 1
      KP1 = LOW + 1
      IF (LA .LT. KP1) GO TO 200
C     .......... FOR MP=IGH-1 STEP -1 UNTIL LOW+1 DO -- ..........
      DO 140 MM = KP1, LA
         MP = LOW + IGH - MM
         MP1 = MP + 1
C
         DO 110 I = MP1, IGH
            XR = AR(I,MP-1)
            XI = AI(I,MP-1)
            IF (XR .EQ. 0.0E0 .AND. XI .EQ. 0.0E0) GO TO 110
C
            DO 100 J = 1, M
               ZR(I,J) = ZR(I,J) + XR * ZR(MP,J) - XI * ZI(MP,J)
               ZI(I,J) = ZI(I,J) + XR * ZI(MP,J) + XI * ZR(MP,J)
  100       CONTINUE
C
  110    CONTINUE
C
         I = INT(MP)
         IF (I .EQ. MP) GO TO 140
C
         DO 130 J = 1, M
            XR = ZR(I,J)
            ZR(I,J) = ZR(MP,J)
            ZR(MP,J) = XR
            XI = ZI(I,J)
            ZI(I,J) = ZI(MP,J)
            ZI(MP,J) = XI
  130    CONTINUE
C
  140 CONTINUE
C
  200 RETURN
      END
