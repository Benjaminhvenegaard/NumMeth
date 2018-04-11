      SUBROUTINE BVDER (X, Y, YP, G, IPAR)
      DIMENSION Y(*),YP(*),G(*)
C
C **********************************************************************
C
      COMMON /ML8SZ/ C,XSAV,IGOFX,INHOMO,IVP,NCOMP,NFC
C
C **********************************************************************
C     The COMMON block below is used to communicate with the user
C     supplied subroutine FMAT.  The user should not alter this
C     COMMON block.
C
      COMMON /MLIVP/ NOFST
C **********************************************************************
C
C***FIRST EXECUTABLE STATEMENT  BVDER
      IF (IVP .GT. 0) CALL UIVP(X,Y(IVP+1),YP(IVP+1))
      NOFST = IVP
      NA = 1
      DO 10 K=1,NFC
         CALL FMAT(X,Y(NA),YP(NA))
         NOFST = NOFST - NCOMP
         NA = NA + NCOMP
   10 CONTINUE
C
      IF (INHOMO .NE. 1) RETURN
      CALL FMAT(X,Y(NA),YP(NA))
C
      IF (IGOFX .EQ. 0) RETURN
      IF (X .NE. XSAV) THEN
         IF (IVP .EQ. 0) CALL GVEC(X,G)
         IF (IVP .GT. 0) CALL UVEC(X,Y(IVP+1),G)
         XSAV = X
      ENDIF
C
C     If the user has chosen not to normalize the particular
C     solution, then C is defined in BVPOR to be 1.0
C
C     The following loop is just
C     CALL SAXPY (NCOMP, 1.0E0/C, G, 1, YP(NA), 1)
C
      DO 20 J=1,NCOMP
         L = NA + J - 1
         YP(L) = YP(L) + G(J)/C
   20 CONTINUE
      RETURN
      END
