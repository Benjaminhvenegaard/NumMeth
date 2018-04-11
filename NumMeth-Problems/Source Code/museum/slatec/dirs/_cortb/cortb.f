      SUBROUTINE CORTB (NM, LOW, IGH, AR, AI, ORTR, ORTI, M, ZR, ZI)
C
      INTEGER I,J,M,LA,MM,MP,NM,IGH,KP1,LOW,MP1
      REAL AR(NM,*),AI(NM,*),ORTR(*),ORTI(*)
      REAL ZR(NM,*),ZI(NM,*)
      REAL H,GI,GR
C
C***FIRST EXECUTABLE STATEMENT  CORTB
      IF (M .EQ. 0) GO TO 200
      LA = IGH - 1
      KP1 = LOW + 1
      IF (LA .LT. KP1) GO TO 200
C     .......... FOR MP=IGH-1 STEP -1 UNTIL LOW+1 DO -- ..........
      DO 140 MM = KP1, LA
         MP = LOW + IGH - MM
         IF (AR(MP,MP-1) .EQ. 0.0E0 .AND. AI(MP,MP-1) .EQ. 0.0E0)
     1      GO TO 140
C     .......... H BELOW IS NEGATIVE OF H FORMED IN CORTH ..........
         H = AR(MP,MP-1) * ORTR(MP) + AI(MP,MP-1) * ORTI(MP)
         MP1 = MP + 1
C
         DO 100 I = MP1, IGH
            ORTR(I) = AR(I,MP-1)
            ORTI(I) = AI(I,MP-1)
  100    CONTINUE
C
         DO 130 J = 1, M
            GR = 0.0E0
            GI = 0.0E0
C
            DO 110 I = MP, IGH
               GR = GR + ORTR(I) * ZR(I,J) + ORTI(I) * ZI(I,J)
               GI = GI + ORTR(I) * ZI(I,J) - ORTI(I) * ZR(I,J)
  110       CONTINUE
C
            GR = GR / H
            GI = GI / H
C
            DO 120 I = MP, IGH
               ZR(I,J) = ZR(I,J) + GR * ORTR(I) - GI * ORTI(I)
               ZI(I,J) = ZI(I,J) + GR * ORTI(I) + GI * ORTR(I)
  120       CONTINUE
C
  130    CONTINUE
C
  140 CONTINUE
C
  200 RETURN
      END
