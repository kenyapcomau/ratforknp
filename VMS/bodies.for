C
C                        THREE-BODY REGULARIZATION PROGRAM
C                        *********************************
C
C          THE METHOD IS DESCRIBED IN AARSETH & ZARE, CELESTIAL MECHANICS 10,185
C
      IMPLICIT  REAL*8  (A-H,M,O-Z)
C          DECLARE M AS A REAL VARIABLE IF IMPLICIT STATEMENT IS REMOVED.
      COMMON  DT0,TOL0,DELTAT,TCRIT,TIME,Q(8),P(8),R,R1,R2,ENERGY,M(3),
     1      X(3,3),XDOT(3,3),C11,C12,C19,C20,C24,C25,NSTEPS,NAME(3),NRJ
      DIMENSION  Y(17),F(17),F1(17),F2(17),F3(17),F4(17),F5(17),F6(17),
     1                                                   F7(17),SAVE(6)
C
      EXTERNAL QDERIV
C          NAME OF DERIVATIVE ROUTINE REQUIRED BY THE RUNGE-KUTTA INTEGRATOR.
C
C                  LIST OF COMMON VARIABLES
C
C          ---------------------------------------------------------------------
C          C11     INVERSE MASS FACTOR FOR QDERIV (ALSO C12,C19,C20,C24,C25).
C          DELTAT  APPROXIMATE OUTPUT TIME INTERVAL.
C          DT0     INITIAL INTEGRATION STEP IN REGULARIZED TIME UNITS.
C          ENERGY  TWICE THE INITIAL TOTAL ENERGY.
C          M       PARTICLE MASS.
C          NAME    PARTICLE IDENTITY (INITIALIZED TO 1,2,3).
C          NRJ     NUMBER OF REJECTED STEPS IN THE INTEGRATOR.
C          NSTEPS  TOTAL NUMBER OF INTEGRATION STEPS.
C          P       REGULARIZED MOMENTA.
C          Q       REGULARIZED COORDINATES.
C          R       DISTANCE BETWEEN M(1) AND M(2).
C          R1      DISTANCE BETWEEN M(1) AND M(3).
C          R2      DISTANCE BETWEEN M(2) AND M(3).
C          TCRIT   EXACT TERMINATION TIME (REACHED BY ITERATION).
C          TIME    PHYSICAL TIME IN SCALED UNITS.
C          TOL0    ABSOLUTE TOLERANCE FOR RUNGE-KUTTA INTEGRATOR.
C          X       PARTICLE COORDINATES (NOTE THAT X(I,3) IS THE Z-COMPONENT).
C          XDOT    VELOCITY COMPONENTS (XDOT(I,3) IS Z-COMPONENT).
C          ---------------------------------------------------------------------
C
C          READ INPUT PARAMETERS AND SPECIFY INITIAL CONDITIONS.
      CALL DATA
      IRUN = 0
C          CONTROL VARIABLE FOR TIME REVERSAL TEST.
   10 R12MIN = 100.0
      RMIN = 100.0
      NSTEPS = 0
      NRJ = 0
      NREG = 0
      IEND = 0
C          CONTROL VARIABLE FOR THE FINAL TIME ITERATION.
      H = DT0
      TOL = TOL0
      TNEXT = DELTAT
C          NEXT OUTPUT TIME TO THE NEAREST STEP.
      TIME = 0.0D0
C          INITIALIZE THE REGULARIZED TIME.
      TAU = 0.0D0
C          SPECIFY THE NUMBER OF FIRST-ORDER EQUATIONS FOR RK INTEGRATOR.
      N = 17
C          COMPUTE INITIAL ENERGY AND TRANSFORM TO REGULARIZED VARIABLES.
      IF(IRUN.EQ.0)  CALL TRANSF(1)
C          INITIALIZE THE INPUT VECTOR FOR RK INTEGRATOR.
      DO 20 K = 1,8
      Y(K) = Q(K)
   20 Y(K+8) = P(K)
      Y(17) = 0.0D0
C          OBTAIN INITIAL OUTPUT AFTER TRANSFORMATION TO PHYSICAL VARIABLES.
      IF (IRUN.EQ.0)  CALL TRANSF(2)
C          CALL THE RUNGE-KUTTA INTEGRATOR FOR NEXT STEP.
C
   30 CALL RK78(I,TAU,H,Y,F,F1,F2,F3,F4,F5,F6,F7,N,TOL,QDERIV)
C
C          SET CURRENT REGULARIZED COORDINATES AND MOMENTA IN COMMON VARIABLES.
      DO 40 K = 1,8
      Q(K) = Y(K)
   40 P(K) = Y(K+8)
      TIME = Y(17)
      IF (IEND.GT.0)  GO TO 80
C          SET MINIMUM TWO-BODY SEPARATIONS (ONLY USED FOR OUTPUT).
      IF (R.LT.RMIN)  RMIN = R
      IF (R1.LT.R12MIN)  R12MIN = R1
      IF (R2.LT.R12MIN)  R12MIN = R2
      NRJ = NRJ + I
      NSTEPS = NSTEPS + 1
C          SEE WHETHER SWITCHING OF REFERENCE BODY IS DESIRABLE.
      IF (R.GT.R1.OR.R.GT.R2)  GO TO 70
      IMIN = 1
C          USE A SIMPLE DISTANCE TEST TO DETERMINE NEW REFERENCE BODY IMIN.
      IF(R2.LT.1.00001*R1) IMIN=2
C          TRANSFORM TO PHYSICAL VARIABLES AND RENAME THE EXCHANGED PARTICLES.
      CALL TRANSF(3)
      DO 50 K = 1,3
      SAVE(K) = X(3,K)
      SAVE(K+3) = XDOT(3,K)
      X(3,K) = X(IMIN,K)
      XDOT(3,K) = XDOT(IMIN,K)
      X(IMIN,K) = SAVE(K)
   50 XDOT(IMIN,K) = SAVE(K+3)
      SAVE(1) = M(3)
      M(3) = M(IMIN)
      M(IMIN) = SAVE(1)
      NAME3 = NAME(3)
      NAME(3) = NAME(IMIN)
      NAME(IMIN) = NAME3
C          TRANSFORM BACK TO REGULARIZED VARIABLES AND INITIALIZE RK INPUT.
      CALL TRANSF(4)
      DO 60 K = 1,8
      Y(K) = Q(K)
   60 Y(K+8) = P(K)
C          UPDATE REGULARIZATION COUNTER AT THE END OF SWITCHING PROCEDURE.
      NREG = NREG + 1
   70 IF (TIME.LT.TNEXT)  GO TO 30
      IF (TIME.GT.TCRIT)  GO TO 80
C          OBTAIN CURRENT OUTPUT AND SEE WHETHER TO CONTINUE THE CALCULATION.
      CALL TRANSF(2)
      TNEXT = TNEXT + DELTAT
      IF (TIME.LT.TCRIT)  GO TO 30
   80 IF (DABS(TIME-TCRIT).LT.1.0E-13)  GO TO 90
C          GUESS THE NEXT REGULARIZED TIME STEP FOR ITERATION TO TIME=TCRIT.
      H = (TCRIT-TIME)/(R1*R2)
      IEND = IEND + 1
C          INCLUDE SAFETY TEST ON THE NUMBER OF ITERATIONS.
      IF (IEND.LE.5)  GO TO 30
   90 CALL TRANSF(2)
      DT = TIME-TCRIT
      WRITE (6,100)  NREG,RMIN,R12MIN,DT,IEND
  100 FORMAT  (1H0//,20X,' NREG =',I3,'     MINIMUM R =',1PE9.1,
     1'    MINIMUM R1 OR R2 =',E9.1,'    DT =',E9.1,'    IEND =',0PI3)
C     IF (IRUN.EQ.0)  STOP
      IF (IRUN.GT.0)  STOP
C          ALTERNATIVE TERMINATION CRITERION FOR TIME REVERSAL TEST.
      TCRIT = TIME
      DO 110 K = 1,8
  110 P(K) = -P(K)
      IRUN = IRUN + 1
      GO TO 10
      END
      SUBROUTINE DATA
      IMPLICIT  REAL*8  (A-H,M,O-Z)
      COMMON  DT0,TOL0,DELTAT,TCRIT,TIME,Q(8),P(8),R,R1,R2,ENERGY,M(3),
     1      X(3,3),XDOT(3,3),C11,C12,C19,C20,C24,C25,NSTEPS,NAME(3),NRJ
      DIMENSION  SUM(7)
C
C          READ INTEGRATION PARAMETERS.
      READ  (5,1)  DT0,TOL0,DELTAT,TCRIT
    1 FORMAT  (8E10.1)
      DO 7 K = 1,7
    7 SUM(K) = 0.0D0
C          READ INITIAL CONDITIONS (ONE CARD FOR EACH BODY).
      DO 2 I = 1,3
    2 READ  (5,3) M(I),(X(I,K),K=1,3),(XDOT(I,K),K=1,3)
    3 FORMAT  (7F10.5)
      DO 4 I = 1,3
      SUM(7) = SUM(7) + M(I)
      DO 4 K = 1,3
      SUM(K) = SUM(K) + M(I)*X(I,K)
    4 SUM(K+3) = SUM(K+3) + M(I)*XDOT(I,K)
C          INITIALIZE NAME AND EXPRESS COORDINATES AND VELOCITIES IN C.M. FRAME.
      DO 5 I = 1,3
      NAME(I) = I
      DO 5 K = 1,3
      X(I,K) = X(I,K) - SUM(K)/SUM(7)
    5 XDOT(I,K) = XDOT(I,K) - SUM(K+3)/SUM(7)
      WRITE  (6,6)  DT0,TOL0,DELTAT,TCRIT,(M(K), K=1,3)
    6 FORMAT  (1H0//,10X,1P7E12.1)
      RETURN
      END
      SUBROUTINE TRANSF(KDUM)
      IMPLICIT  REAL*8  (A-H,M,O-Z)
      COMMON  DT0,TOL0,DELTAT,TCRIT,TIME,Q(8),P(8),R,R1,R2,ENERGY,M(3),
     1      X(3,3),XDOT(3,3),C11,C12,C19,C20,C24,C25,NSTEPS,NAME(3),NRJ
      DIMENSION  A(3,3),P2(9),Q2(9),Q1(9),P1(9)
C
      GO TO  (1,20,20,3),  KDUM
    1 ZKE = 0.0D0
      POT = 0.0D0
      DO 2 I = 1,3
      ZKE = ZKE + 0.5D0*M(I)*(XDOT(I,1)**2 + XDOT(I,2)**2+XDOT(I,3)**2)
      DO 2 J = 1,3
      IF (J.LE.I)  GO TO 2
      POT = POT - M(I)*M(J)/DSQRT((X(I,1)-X(J,1))**2 +(X(I,2)-X(J,2))**2
     1                                             + (X(I,3)-X(J,3))**2)
    2 CONTINUE
      ENERGY = ZKE + POT
C          STORE TWICE THE INITIAL ENERGY IN COMMON TO SAVE TIME IN QDERIV.
      ENERGY = 2.0D0*ENERGY
C          FIRST PART TRANSFORMS PHYSICAL VARIABLES TO REGULARIZED VARIABLES.
    3 DO 4 I = 1,3
C          OBTAIN PHYSICAL MOMENTA AND COORDINATES.
      DO 4 K = 1,3
      I1 = 3*I + K - 3
      P1(I1) = M(I)*XDOT(I,K)
    4 Q1(I1) = X(I,K)
C          SET MASS FACTORS FOR ROUTINE QDERIV TO SAVE TIME.
      C11 = 0.25D0/M(1) + 0.25D0/M(3)
      C12 = 0.25D0/M(2) + 0.25D0/M(3)
      C19 = 2.0D0*M(2)*M(3)
      C20 = 2.0D0*M(1)*M(3)
      C24 = 0.25D0/M(3)
      C25 = 2.0D0*M(1)*M(2)
C          OBTAIN RELATIVE COORDINATES AND ABSOLUTE MOMENTA (EQUATION (45)).
      DO 5 K = 1,3
      P2(K+3) = P1(K+3)
      Q2(K) = Q1(K) - Q1(K+6)
    5 Q2(K+3) = Q1(K+3) - Q1(K+6)
C          EXPAND THE VARIABLES BY RE-LABELLING (EQUATION (46)).
      DO 6 K = 1,3
      Q1(K) = Q2(K)
      P1(K+4) = P2(K+3)
    6 Q1(K+4) = Q2(K+3)
C          INITIALIZE THE REDUNDANT VARIABLES (EQUATION (47)).
      Q1(4) = 0.0D0
      Q1(8) = 0.0D0
      P1(4) = 0.0D0
      P1(8) = 0.0D0
      K = 0
C          PERFORM THE FIRST KS TRANSFORMATION.
    7 RK = DSQRT(Q1(K+1)**2 + Q1(K+2)**2 + Q1(K+3)**2)
      IF (Q1(K+1).GT.0.0)  GO TO 8
C          SET KS COORDINATES FOR NEGATIVE Q1 (EQUATION (48)).
      Q(K+2) = DSQRT (0.5D0*(RK - Q1(K+1)))
      Q(K+1) = 0.5D0*Q1(K+2)/Q(K+2)
      Q(K+4) = 0.5D0*Q1(K+3)/Q(K+2)
      Q(K+3) = 0.0D0
      GO TO 9
C          SET KS COORDINATES FOR POSITIVE Q1 (EQUATION (49)).
    8 Q(K+1) = DSQRT (0.5D0*(RK + Q1(K+1)))
      Q(K+2) = 0.5D0*Q1(K+2)/Q(K+1)
      Q(K+3) = 0.5D0*Q1(K+3)/Q(K+1)
      Q(K+4) = 0.0D0
C          SET REGULARIZED MOMENTA (EQUATION (50)).
    9 P(K+1) =2.D0*(+Q(K+1)*P1(K+1) + Q(K+2)*P1(K+2) + Q(K+3)*P1(K+3))
      P(K+2) =2.D0*(-Q(K+2)*P1(K+1) + Q(K+1)*P1(K+2) + Q(K+4)*P1(K+3))
      P(K+3) =2.D0*(-Q(K+3)*P1(K+1) - Q(K+4)*P1(K+2) + Q(K+1)*P1(K+3))
      P(K+4) =2.D0*(+Q(K+4)*P1(K+1) - Q(K+3)*P1(K+2) + Q(K+2)*P1(K+3))
      IF (K.GT.0)  RETURN
C
      K = 4
C          PERFORM THE SECOND KS TRANSFORMATION.
      GO TO 7
C
C          THIS PART TRANSFORMS REGULARIZED VARIABLES TO PHYSICAL VARIABLES.
   20 K = 0
   21 Q1(K+1) = Q(K+1)**2 - Q(K+2)**2 - Q(K+3)**2 + Q(K+4)**2
      Q1(K+2) =2.D0*(Q(K+1)*Q(K+2) - Q(K+3)*Q(K+4))
      Q1(K+3) =2.D0*(Q(K+1)*Q(K+3) + Q(K+2)*Q(K+4))
C          RELATIVE COORDINATES (EQUATION (52)).
      P1(K+1) = Q(K+1)*P(K+1)-Q(K+2)*P(K+2)-Q(K+3)*P(K+3)+Q(K+4)*P(K+4)
      P1(K+2) = Q(K+2)*P(K+1)+Q(K+1)*P(K+2)-Q(K+4)*P(K+3)-Q(K+3)*P(K+4)
      P1(K+3) = Q(K+3)*P(K+1)+Q(K+4)*P(K+2)+Q(K+1)*P(K+3)+Q(K+2)*P(K+4)
C          THE FACTOR 2 IN THE TRANSPOSE MATRIX IS INCLUDED BELOW.
      RK = Q(K+1)**2 + Q(K+2)**2 + Q(K+3)**2 + Q(K+4)**2
      DK = 4.0D0*RK
      P1(K+1) = 2.0D0*P1(K+1)/DK
      P1(K+2) = 2.0D0*P1(K+2)/DK
      P1(K+3) = 2.0D0*P1(K+3)/DK
C          ABSOLUTE MOMENTA (EQUATION (53)).
      IF (K.GT.0)  GO TO 24
      K = 4
      GO TO 21
C
   24 DO 25 K = 1,3
      Q1(K+3) = Q1(K+4)
   25 P1(K+3) = P1(K+4)
C          RE-LABELLED RELATIVE COORDINATES AND MOMENTA (EQUATION (54)).
      DO 26 K = 1,3
      Q2(K+6) = -(M(1)*Q1(K) + M(2)*Q1(K+3))/(M(1) + M(2) + M(3))
C          PHYSICAL COORDINATES OF M(3) (FIRST EQUATION (55)).
      Q2(K) = Q1(K) + Q2(K+6)
      Q2(K+3) = Q1(K+3) + Q2(K+6)
      P2(K) = P1(K)
      P2(K+3) = P1(K+3)
   26 P2(K+6) = -(P2(K) + P2(K+3))
C          PHYSICAL COORDINATES AND MOMENTA IN C.M. FRAME (EQUATION (55)).
      DO 27 I = 1,3
      DO 27 K = 1,3
      I1 = 3*I + K - 3
      X(I,K) = Q2(I1)
   27 XDOT(I,K) = P2(I1)/M(I)
C          INDIVIDUAL COORDINATES AND VELOCITIES IN THE C.M. FRAME.
      IF (KDUM.EQ.3)    RETURN
C
C          THIS PART IS FOR OUTPUT ONLY.
      S1 = P2(1)**2 + P2(2)**2 + P2(3)**2
      S2 = P2(4)**2 + P2(5)**2 + P2(6)**2
      S3 = P2(7)**2 + P2(8)**2 + P2(9)**2
      ZKE = 0.5D0*(S1/M(1) + S2/M(2) + S3/M(3))
      S1 = M(1)*M(3)/DSQRT((Q2(7)-Q2(1))**2 + (Q2(8)-Q2(2))**2 +
     1                                        (Q2(9)-Q2(3))**2)
      S2 = M(2)*M(3)/DSQRT((Q2(7)-Q2(4))**2 + (Q2(8)-Q2(5))**2 +
     1                                        (Q2(9)-Q2(6))**2)
      S3 = M(1)*M(2)/DSQRT((Q2(4)-Q2(1))**2 + (Q2(5)-Q2(2))**2 +
     1                                        (Q2(6)-Q2(3))**2)
      HT = ZKE - S1 - S2 - S3
C          CURRENT TOTAL ENERGY COMPUTED FROM PHYSICAL VARIABLES.
      DH = (HT - 0.5D0*ENERGY)/(0.5D0*ENERGY)
C          RELATIVE ENERGY ERROR WITH RESPECT TO INITIAL VALUE.
      WRITE  (6,30)  TIME,HT,DH,NSTEPS,NRJ
   30 FORMAT  (1H0,F30.15,F15.10,E15.2,2I10)
      DO 32 I = 1,3
   32 WRITE (6,31)  NAME(I),M(I),(X(I,K),K=1,3),(XDOT(I,K),K=1,3)
   31 FORMAT  (1H0,10X,I3,F8.2,3X,3F17.12,3X,3F15.10)
      RETURN
      END
      SUBROUTINE QDERIV(Q,P,T,QPR,PPR,TPR)
      IMPLICIT  REAL*8  (A-H,M,O-Z)
      COMMON  DT0,TOL0,DELTAT,TCRIT,TIME,V(8),W(8),R,R1,R2,ENERGY,M(3),
     1      X(3,3),XDOT(3,3),C11,C12,C19,C20,C24,C25,NSTEPS,NAME(3),NRJ
C          NOTE THAT COMMON LOCATIONS OF Q AND P ARE REPLACED BY DUMMY VARIABLES
      DIMENSION  Q(8),P(8),QPR(8),PPR(8),S2(8),S5(8),S8(8)
C
      R1=Q(1)*Q(1)+Q(2)*Q(2)+Q(3)*Q(3)+Q(4)*Q(4)
      R2=Q(5)*Q(5)+Q(6)*Q(6)+Q(7)*Q(7)+Q(8)*Q(8)
      C3=Q(1)*P(1)-Q(2)*P(2)-Q(3)*P(3)+Q(4)*P(4)
      C4=Q(5)*P(5)-Q(6)*P(6)-Q(7)*P(7)+Q(8)*P(8)
      C5=Q(2)*P(1)+Q(1)*P(2)-Q(4)*P(3)-Q(3)*P(4)
      C6=Q(6)*P(5)+Q(5)*P(6)-Q(8)*P(7)-Q(7)*P(8)
      C7=Q(3)*P(1)+Q(4)*P(2)+Q(1)*P(3)+Q(2)*P(4)
      C8=Q(7)*P(5)+Q(8)*P(6)+Q(5)*P(7)+Q(6)*P(8)
      C9=P(1)*P(1)+P(2)*P(2)+P(3)*P(3)+P(4)*P(4)
      C10=P(5)*P(5)+P(6)*P(6)+P(7)*P(7)+P(8)*P(8)
      C13 = C11*R2
      C14 = C12*R1
      C15=C12*C10
      C16=C11*C9
      C17 = R2*ENERGY
      C18 = R1*ENERGY
C          NOTE THAT TWICE THE ENERGY IS STORED IN COMMON.
      C21=Q(1)*Q(1)-Q(2)*Q(2)-Q(3)*Q(3)+Q(4)*Q(4)
     1-Q(5)*Q(5)+Q(6)*Q(6)+Q(7)*Q(7)-Q(8)*Q(8)
      C22 =   Q(1)*Q(2)-Q(3)*Q(4)-Q(5)*Q(6)+Q(7)*Q(8)
      C23 =   Q(1)*Q(3)+Q(2)*Q(4)-Q(5)*Q(7)-Q(6)*Q(8)
      C22 = C22 + C22
      C23 = C23 + C23
      RR = C21*C21 + C22*C22 + C23*C23
      R = DSQRT (RR)
      A = C25/R
      TPR = R1*R2
C          FIRST DERIVATIVE OF THE PHYSICAL TIME.
      B = A*TPR/RR
      S2(1)=Q(1)*C4+Q(2)*C6+Q(3)*C8
      S2(2)=-Q(2)*C4+Q(1)*C6+Q(4)*C8
      S2(3)=-Q(3)*C4-Q(4)*C6+Q(1)*C8
      S2(4)=Q(4)*C4-Q(3)*C6+Q(2)*C8
      S2(5)=Q(5)*C3+Q(6)*C5+Q(7)*C7
      S2(6)=-Q(6)*C3+Q(5)*C5+Q(8)*C7
      S2(7)=-Q(7)*C3-Q(8)*C5+Q(5)*C7
      S2(8)=Q(8)*C3-Q(7)*C5+Q(6)*C7
      S5(1)=P(1)*C4+P(2)*C6+P(3)*C8
      S5(2)=-P(2)*C4+P(1)*C6+P(4)*C8
      S5(3)=-P(3)*C4-P(4)*C6+P(1)*C8
      S5(4)=P(4)*C4-P(3)*C6+P(2)*C8
      S5(5)=P(5)*C3+P(6)*C5+P(7)*C7
      S5(6)=-P(6)*C3+P(5)*C5+P(8)*C7
      S5(7)=-P(7)*C3-P(8)*C5+P(5)*C7
      S5(8)=P(8)*C3-P(7)*C5+P(6)*C7
      S8(1) = Q(1)*C21 + Q(2)*C22 + Q(3)*C23
      S8(2) = -Q(2)*C21 + Q(1)*C22 + Q(4)*C23
      S8(3) = -Q(3)*C21 -Q(4)*C22 + Q(1)*C23
      S8(4) = Q(4)*C21 - Q(3)*C22 + Q(2)*C23
      S8(5) = -Q(5)*C21 - Q(6)*C22 - Q(7)*C23
      S8(6) = Q(6)*C21 - Q(5)*C22 - Q(8)*C23
      S8(7) = Q(7)*C21 + Q(8)*C22 - Q(5)*C23
      S8(8) = -Q(8)*C21 + Q(7)*C22 - Q(6)*C23
      C1 = C17 - C15 + C19 + A*R2
      C2 = C18 - C16 + C20 + A*R1
C          THE DERIVATIVES OF THE REGULARIZED HAMILTONIAN ARE DENOTED QPR & PPR.
      DO 10 I = 1,4
      K = I + 4
      QPR(I) = C13*P(I) + C24*S2(I)
      QPR(K) = C14*P(K) + C24*S2(K)
      PPR(I) = C1*Q(I) - C24*S5(I) - B*S8(I)
      PPR(K) = C2*Q(K) - C24*S5(K) - B*S8(K)
   10 CONTINUE
      RETURN
      END
      SUBROUTINE RK78(IREJCT,T,DT,X,XDUM,F1,F2,F3,F4,F5,F6,F7,N,TOL,DER)
C
      IMPLICIT  REAL*8  (A-H,O-Z)
C
C
C THIS SUBROUTINE WILL INTEGRATE A SYSTEM OF FIRST ORDER DIFF EQS USING
C THE RUNGE-KUTTA-FEHLBERG 7(8) FORMULA (NASA TECHNICAL REPORT TR R-287)
C
C IT CONTAINS AN AUTOMATIC STEPSIZE CONTROL, BUT AN INITIAL STEPSIZE
C MUST BE SPECIFIED. THE CALLING PROGRAM MUST CALL RK7(8) ONCE PER STEP.
C IF DT IS TOO LARGE, THE STEP IS REJECTED AND AUTOMATICALLY RECOMPUTED.
C DESCRIPTION OF PARAMETERS IN ARGUMENT LIST,
C
C IREJCT - RETURNED AS 1 IF A STEP WAS REJECTED, OTHERWISE 0.
C T - INDEPENDENT VARIABLE.
C DT - STEPSIZE.
C X - ARRAY OF DEPENDENT VARIABLES, DIMENSIONED N.
C XDUM,F1,F2,F3,F4,F5,F6,F7 - INTERNALLY USED ARRAYS, DIMENSIONED N.
C N - NUMBER OF DIFFERENTIAL EQUATIONS.
C TOL - LARGEST ALLOWED TRUNCATION ERROR AT EACH STEP.
C DER - NAME OF THE DERIVATIVE SUBROUTINE
C
C THE DERIVATIVE ROUTINE SHOULD BE DEFINED AS FOLLOWS,
C     SUBROUTINE DER(T,X,XDOT)
C
C
      DIMENSION X(N),XDUM(N),F1(N),F2(N),F3(N),F4(N),F5(N),F6(N),F7(N)
      DIMENSION CH(13),ALPH(13)
      LOGICAL  FIRST/.TRUE./
C
      IF (.NOT.FIRST)  GO TO 2
      CH(1) = 0.0
      CH(2) = 0.0
      CH(3) = 0.0
      CH(4) = 0.0
      CH(5) = 0.0
      CH(6)=34./105.
      CH(7)=9./35.
      CH(8)=CH(7)
      CH(9)=9./280.
      CH(10)=CH(9)
      CH(11) = 0.0
      CH(12)=41./840.
      CH(13)=CH(12)
      ALPH(1) = 0.0
      ALPH(2)=2./27.
      ALPH(3)=1./9.
      ALPH(4)=1./6.
      ALPH(5)=5./12.
      ALPH(6)=.5
      ALPH(7)=5./6.
      ALPH(8)=1./6.
      ALPH(9)=2./3.
      ALPH(10)=1./3.
      ALPH(11)=1.
      ALPH(12) = 0.0
      ALPH(13)=1.
      B2 1 = 2./27.
      B3 1 = 1./36.
      B4 1 = 1./24.
      B5 1 = 5./12.
      B6 1 = .05
      B7 1 = -25./108.
      B8 1 = 31./300.
      B9 1 = 2.
      B10 1= -91./108.
      B11 1 = 2383./4100.
      B12 1 = 3./205.
      B13 1 = -1777./4100.
      B3 2  = 1./12.
      B4 3  = 1./8.
      B5 3  = -25./16.
      B5 4 = -B5 3
      B6 4  = .25
      B7 4  = 125./108.
      B9 4  = -53./6.
      B10 4 = 23./108.
      B11 4 = -341./164.
      B13 4 = B114
      B6 5  = .2
      B7 5  = -65./27.
      B8 5  = 61./225.
      B9 5  = 704./45.
      B10 5 = -976./135.
      B11 5 = 4496./1025.
      B1 35 = B1 15
      B7 6  = 125./54.
      B8 6  = -2./9.
      B9 6  = -107./9.
      B10 6 = 311./54.
      B11 6 = -301./82.
      B12 6 = -6./41.
      B13 6 = -289./82.
      B8 7  = 13./900.
      B9 7  = 67./90.
      B10 7 = -19./60.
      B11 7 = 2133./4100.
      B12 7 = -3./205.
      B13 7 = 2193./4100.
      B9 8  = 3.
      B10 8 = 17./6.
      B11 8 = 45./82.
      B12 8 = -3./41.
      B13 8 = 51./82.
      B10 9 = -1./12.
      B11 9 = 45./164.
      B12 9 = 3./41.
      B13 9 = 33./164.
      B11 10= 18./41.
      B12 10= 6./41.
      B13 10= 12./41.
      B13 12= 1.
      EXPT = 1.0/16.0
      FIRST = .FALSE.
      TOL2 = TOL*TOL
    2 IREJCT = 0
      CALL DER(X(1),X(9),X(17),F1(1),F1(9),F1(17))
    3 CONTINUE
      DO 71 I=1,N
   71 XDUM(I) = X(I) + DT*B2 1*F1(I)
      CALL DER(XDUM(1),XDUM(9),XDUM(17),F2(1),F2(9),F2(17))
      DO 72 I=1,N
   72 XDUM(I) = X(I) + DT*(B3 1*F1(I)+B3 2*F2(I))
      CALL DER(XDUM(1),XDUM(9),XDUM(17),F3(1),F3(9),F3(17))
      DO 73 I=1,N
  73   XDUM(I) = X(I)+DT*(B4 1*F1(I)+B4 3*F3(I))
      CALL DER(XDUM(1),XDUM(9),XDUM(17),F4(1),F4(9),F4(17))
      DO 74 I=1,N
   74 XDUM(I) = X(I)+DT*(B5 1*F1(I)+B5 3*F3(I)+B5 4*F4(I))
      CALL DER(XDUM(1),XDUM(9),XDUM(17),F5(1),F5(9),F5(17))
      DO 75 I=1,N
   75 XDUM(I) = X(I)+DT*(B6 1*F1(I)+B6 4*F4(I)+B6 5*F5(I))
      CALL DER(XDUM(1),XDUM(9),XDUM(17),F6(1),F6(9),F6(17))
      DO 76 I=1,N
   76 XDUM(I)=X(I)+DT*(B7 1*F1(I)+B7 4*F4(I)+B7 5*F5(I)+B7 6*F6(I))
      CALL DER(XDUM(1),XDUM(9),XDUM(17),F7(1),F7(9),F7(17))
      DO 77 I=1,N
   77 XDUM(I)=X(I)+DT*(B8 1*F1(I)+B8 5*F5(I)+B8 6*F6(I)+B8 7*F7(I))
      CALL DER(XDUM(1),XDUM(9),XDUM(17),F2(1),F2(9),F2(17))
      DO 78 I=1,N
   78 XDUM(I)=X(I)+DT*(B9 1*F1(I)+B9 4*F4(I)+B9 5*F5(I)+B9 6*F6(I)
     C +B9 7*F7(I)+B9 8*F2(I))
      CALL DER(XDUM(1),XDUM(9),XDUM(17),F3(1),F3(9),F3(17))
      DO 82 I=1,N
      X4 = F4(I)
      X5 = F5(I)
      X6 = F6(I)
      X7 = F7(I)
      X8 = F2(I)
      X9 = F3(I)
      F2(I) = CH(6)*X6+CH(7)*X7+CH(8)*X8+CH(9)*X9
      XDUM(I)=X(I)+DT*(B10 1*F1(I)+B10 4*X4+B10 5*X5+B10 6*X6+B10 7*X7 +
     C B10 8*X8+B10 9*X9)
      F4(I)=B11 1*F1(I)+B11 4*X4+B11 5*X5+B11 6*X6+B11 7*X7+B11 8*X8
     C +B11 9*X9
      F5(I)=B12 1*F1(I)+B12 6*X6+B12 7*X7+B12 8*X8+B12 9*X9
   82 F6(I)=B13 1*F1(I)+B13 4*X4+B13 5*X5+B13 6*X6+B13 7*X7+
     C B13 8*X8+B13 9*X9
      CALL DER(XDUM(1),XDUM(9),XDUM(17),F3(1),F3(9),F3(17))
      DO 83 I=1,N
   83 XDUM(I)=X(I)+DT*(F4(I)+B11 10*F3(I))
      CALL DER(XDUM(1),XDUM(9),XDUM(17),F4(1),F4(9),F4(17))
      DO 84 I=1,N
   84 XDUM(I)=X(I)+DT*(F5(I)+B12 10*F3(I))
      CALL DER(XDUM(1),XDUM(9),XDUM(17),F5(1),F5(9),F5(17))
      DO 85 I=1,N
   85 XDUM(I)=X(I)+DT*(F6(I)+B13 10*F3(I)+B13 12*F5(I))
      CALL DER(XDUM(1),XDUM(9),XDUM(17),F6(1),F6(9),F6(17))
      ER2 = 0.0
      DO 112 I = 1,N
      TE = DT*(F1(I)+F4(I)-F5(I)-F6(I))*CH(12)
      ER2 = TE*TE + ER2
  112 CONTINUE
      DT1 = DT
      QQ = TOL2/(400.0*ER2)
      DT = DT1*QQ**EXPT
      IF(ER2.LT.TOL2) GO TO 120
      IREJCT = 1
      GO TO 3
  120 T=T+DT1
      DO 86 I=1,N
   86 X(I)=X(I)+DT1*(CH(10)*F3(I)+CH(11)*F4(I)+CH(12)*F5(I)
     C + CH(13)*F6(I)+F2(I))
      RETURN
C
      END
