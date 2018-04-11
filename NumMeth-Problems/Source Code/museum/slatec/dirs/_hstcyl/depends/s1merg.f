      SUBROUTINE S1MERG (TCOS, I1, M1, I2, M2, I3)
      INTEGER I1, I2, I3, M1, M2
      REAL TCOS(*)
C
      INTEGER J1, J2, J3
C
C***FIRST EXECUTABLE STATEMENT  S1MERG
      IF (M1.EQ.0 .AND. M2.EQ.0) RETURN
C
      IF (M1.EQ.0 .AND. M2.NE.0) THEN
         CALL SCOPY (M2, TCOS(I2+1), 1, TCOS(I3+1), 1)
         RETURN
      ENDIF
C
      IF (M1.NE.0 .AND. M2.EQ.0) THEN
         CALL SCOPY (M1, TCOS(I1+1), 1, TCOS(I3+1), 1)
         RETURN
      ENDIF
C
      J1 = 1
      J2 = 1
      J3 = 1
C
   10 IF (TCOS(I1+J1) .LE. TCOS(I2+J2)) THEN
         TCOS(I3+J3) = TCOS(I1+J1)
         J1 = J1+1
         IF (J1 .GT. M1) THEN
            CALL SCOPY (M2-J2+1, TCOS(I2+J2), 1, TCOS(I3+J3+1), 1)
            RETURN
         ENDIF
      ELSE
         TCOS(I3+J3) = TCOS(I2+J2)
         J2 = J2+1
         IF (J2 .GT. M2) THEN
            CALL SCOPY (M1-J1+1, TCOS(I1+J1), 1, TCOS(I3+J3+1), 1)
            RETURN
         ENDIF
      ENDIF
      J3 = J3+1
      GO TO 10
      END
