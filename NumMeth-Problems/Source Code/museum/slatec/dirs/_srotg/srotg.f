      SUBROUTINE SROTG (SA, SB, SC, SS)
C***FIRST EXECUTABLE STATEMENT  SROTG
      IF (ABS(SA) .LE. ABS(SB)) GO TO 10
C
C *** HERE ABS(SA) .GT. ABS(SB) ***
C
      U = SA + SA
      V = SB / U
C
C     NOTE THAT U AND R HAVE THE SIGN OF SA
C
      R = SQRT(0.25E0 + V**2) * U
C
C     NOTE THAT SC IS POSITIVE
C
      SC = SA / R
      SS = V * (SC + SC)
      SB = SS
      SA = R
      RETURN
C
C *** HERE ABS(SA) .LE. ABS(SB) ***
C
   10 IF (SB .EQ. 0.0E0) GO TO 20
      U = SB + SB
      V = SA / U
C
C     NOTE THAT U AND R HAVE THE SIGN OF SB
C     (R IS IMMEDIATELY STORED IN SA)
C
      SA = SQRT(0.25E0 + V**2) * U
C
C     NOTE THAT SS IS POSITIVE
C
      SS = SB / SA
      SC = V * (SS + SS)
      IF (SC .EQ. 0.0E0) GO TO 15
      SB = 1.0E0 / SC
      RETURN
   15 SB = 1.0E0
      RETURN
C
C *** HERE SA = SB = 0.0 ***
C
   20 SC = 1.0E0
      SS = 0.0E0
      RETURN
C
      END
