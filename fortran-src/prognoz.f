      PROGRAM PROGNOZ
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         DIMENSION U(51,61,9),W(51,61,9),SOST(4,61),X(51),Z(61)
         DIMENSION FB(61,2),FN(51,2),XR(51),ZR(61),G(51,61)
         DIMENSION ISUT(61),VREM(42),SINT(51,61)
         COMMON /UWM/U,W/RAZM/X,Z,B/XRAB/XR,H,PANAP/A7/A(7)
         COMMON /SILA/FB,FN/GG/S0,G0,STEKO,G,SINT/RABOT/SOST,ARP
         COMMON /SUXLCI/SUX(61,42),SUT(61,42),LCI,JSUX
         COMMON /NXZ/NX1,NX,KX,NZ1,NZ,KZ,N,N1
         DATA PI/3.14159/

         REAL :: RAD, ALPHA, PROP, VS, HDELO, DE, EPXA, EPRA, EPG, EPS, APL, BPLM,
     ;   BPLB, HPL, QPL, CSTEK, DSTEK, CRAST, H, TAU, V, STEKO, PLOTN
         INTEGER :: MAT, KPOVT, NX1, NZ1, JSUX, LCIO

         READ(*,*) RAD
         READ(*,*) ALPHA
         READ(*,*) PROP

         READ(*,*) VS
         READ(*,*) HDELO
         READ(*,*) DE
         READ(*,*) MAT
         READ(*,*) KPOVT

         READ(*,*) EPXA
         READ(*,*) EPRA
         READ(*,*) EPG
         READ(*,*) EPS

         READ(*,*) APL
         READ(*,*) BPLM
         READ(*,*) BPLB
         READ(*,*) HPL
         READ(*,*) QPL

         READ(*,*) CSTEK
         READ(*,*) DSTEK
         READ(*,*) CRAST
         READ(*,*) PLOTN

         READ(*,*) NX1
         READ(*,*) NZ1
         READ(*,*) JSUX
         READ(*,*) LCIO
         READ(*,*) H
         READ(*,*) TAU
         READ(*,*) V
         READ(*,*) STEKO

         WRITE(*,*) 'PARAMETERS END'
         STEK=STEKO*PROP
         ETEK=0.008*PROP
         E0=STEK/ETEK
         STMAX=STEK*CSTEK
         HPL2=HPL/2.0
         RAST=CRAST*STEK
         S0=E0/(1.0-2.0*V)
         G0=E0/(2.0+2.0*V)

         A0=VS*VS*4.0E-9*PLOTN*PI*RAD**3/6.0
         KX=NX1-2
         NX=NX1-1
         KZ=NZ1-2
         NZ=NZ1-1
         DO 99 I=1,NX1
   99    XR(I)=(I-1)*H/2.0
         JN=LCIO+1
         DO 90 J=1,NZ1
            ZR(J)=(J-1)*H/2.0
            ISUT(J)=1
            DO 90 I=1,JN
               SUX(J,I)=0.0
   90    SUT(J,I)=STEK
         ARPO=0.0
         ARP=0.0
         VREM(1)=0.0
         BPR=0.0
         LM=0
         JKO=21
         DO 98 I=1,NX1
            DO 98 J=1,NZ1
   98    G(I,J)=G0
         DO 139 LCI=1,LCIO
            HDEL=HDELO
            LCIK=LCI+1
            VREM(LCIK)=VREM(LCI)+TAU
            IF(LM.EQ.1) GOTO 160
            LM=0
            IPRI=1
   97       LM=LM+1
            B=SQRT(RAD*DE)
            N1=0
            DO 130 I=1,NX1
               IF(XR(I).LE.B) GOTO 130
               N1=I
               GOTO 131
  130       CONTINUE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  131       IF(N1.GE.8) GOTO 132
            GOTO 135
  132       IF(3*N1.LE.NX1) GOTO 136
            ! PRINT 133
            ! WRITE (*,133)
            GOTO 135
  136       N=N1-1
            DO 91 J=1,NZ1
   91       Z(J)=ZR(J)/B
            DO 95 I=1,NX1
   95       X(I)=XR(I)/B
            P=8.0*G0*B*DE/(3.0-3.0*V)
            PANAP=0.2387324*P*(1.0-2.0*V)/(G0*B*B)
            CALL VSP(P,V,H,5,6)
            CALL PERE(1,P,V,G0)
            DO 16 I=1,NX1
               DO 16 J=1,NZ1
                  U(I,J,3)=U(I,J,5)
                  W(I,J,3)=W(I,J,5)
                  U(I,J,4)=U(I,J,6)
   16       W(I,J,4)=W(I,J,6)
  160       ITER=0
            DO 94 J=1,NZ1
               DO 94 I=1,4
   94       SOST(I,J)=0.0
            DO 5 NA=1,20
               A(6)=0.0
               CALL MODG(EG,NX1,NZ1,3,4,MAT)
               CALL NAPR(1,3,4)
               CALL MODG(EG,NX1,NZ1,3,4,MAT)
               ITER=ITER+1
               CALL PDV(1)
               EPS1=EPS*A(6)
               CALL NAPR(1,3,4)
               DO 10 IN=2,25
                  ITER=ITER+1
                  CALL PDV(2)
                  CALL NAPR(1,3,4)
                  IF(ABS(A(7)/A(6)-1.0).LT.EPXA) GOTO 17
                  IF(A(6).LT.EPS1) GOTO 17
   10          CONTINUE
               IF(ABS(A(7)/A(6)-1.0).GT.EPXA) GOTO 5
   17          IF(EG.LT.EPG) GOTO 92
    5       CONTINUE
   92       R1=ARP
            CALL ENER(MAT,3,4,KX,KZ,ARP)
            WRITE (*,*) LCI,N,LM,ITER,ARP,DE,B,P
            ! PRINT 47, LCI,N,LM,ITER,ARP,DE,B,P
   ! 47       FORMAT (4I4,F11.6,2F11.5,F11.2)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            R2=ARP-ARPO
            IF(R2.GT.0.00001) GOTO 72
            GOTO 135
   72       IF((ARP-R2)*(B-BPR)+1.0E-10.GT.0.0) GOTO 74
            DO 66 I=1,NX1
               DO 66 J=1,NZ1
   66       G(I,J)=G0
            BPR=B
            IPRI=1
            GOTO 97
   74       R1=R2/A0
            IF(ABS(R1-1.0).LE.EPRA) GOTO 70
            BPR=B
            IF(IPRI.GT.0) GOTO 71
            HDEL=HDEL/2.0
            DE=DE+HDEL
            IF(A0.LT.R2) DE=DE-2.0*HDEL
            GOTO 97
   71       IF(A0.LT.R2) GOTO 73
            DE=DE+HDEL
            GOTO 97
   73       IPRI=-1
            HDEL=HDEL/2.0
            DE=DE-HDEL
            GOTO 97
   70       IB=1
            JN=1
            DO 211 J=1,KZ
               DO 211 I=1,KX
                  IF(G0-G(I,J).LT.1.0) GOTO 211
                  IF(IB.GE.I) GOTO 212
                  IB=I
  212             IF(JN.GE.J) GOTO 211
                  JN=J
  211       CONTINUE
            R2=QPL*PI*ALPHA*TAU
            DO 112 J=1,JN
               IF(ISUT(J).EQ.0) GOTO 112
               IM=1
               DO 161 I=1,N1
                  IF(W(I,J,9)-SUT(J,LCI).LT.0.0) GOTO 161
                  IM=1
                  GOTO 163
  161          CONTINUE
               GOTO 112
  163          DO 165 I=IM,IB
                  IF(W(I,J,9)-SUT(J,LCI).GE.0.0) GOTO 165
                  IP=I
                  GOTO 167
  165          CONTINUE
               IP=IB+1
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  167          IP=IP-1
               IF(IP.EQ.IM) GOTO 112
               R3=0.0
               DO 168 I=IM,IP
  168          R3=R3+XR(I)*W(I,J,9)+XR(I+1)*W(I+1,J,9)
               R1=R2*(R3*H*2.-SUT(J,LCI)*(XR(IP+1)-XR(IM+1))*(XR(IP+1)
     ;         +XR(IM+1)))
               IF(R1.GT.DSTEK) R1=DSTEK
               SUT(J,LCIK)=SUT(J,LCI)+R1
  112       CONTINUE
            DO 113 J=1,KZ
               IF(ISUT(J).EQ.0) GOTO 115
               IF(SUT(J,LCIK).LT.STMAX) GOTO 114
               ISUT(J)=0
  115          SUT(J,LCIK)=STMAX
               GOTO 113
  114          IF(SUT(J,LCIK).LT.SUT(J,LCI)) SUT(J,LCIK)=SUT(J,LCI)
  113       CONTINUE
            DO 67 J=1,NZ1
               DO 67 I=1,NX1
                  U(I,J,7)=U(I,J,3)-U(I,J,5)
                  W(I,J,7)=W(I,J,3)-W(I,J,5)
                  U(I,J,8)=U(I,J,4)-U(I,J,6)
   67       W(I,J,8)=W(I,J,4)-W(I,J,6)
            CALL NOST(7,8,PAR1,PAR2,IKO,JKO,H)
            R=4.0*QPL*TAU
            DO 4 J=1,KZ
    4       SUX(J,LCIK)=SUX(J,LCI)+R*(SOST(1,J)+SOST(3,J))
            IF(SUX(1,LCIK).GT.RAST) SUX(1,LCIK)=RAST
            IF(SUX(2,LCIK).GT.RAST) SUX(2,LCIK)=RAST
            R1=(HPL2*SUX(1,LCIK)-(HPL2-ZR(JKO))*SUX(JKO,LCIK))/2.0
            DO 49 J=3,JKO,2
   49       R1=R1+2.0*(HPL2-ZR(J-1))*SUX(J-1,LCIK)+(HPL2-ZR(J))*
     ;      SUX(J,LCIK)
            R1=R1*BPLM*APL*H/(2.0*E0*HPL**3)
            ! PRINT 46, LIC,N,LM,ITER,ARP,DE,B,P,IB,JN,IKO,JKO,PAR2,R1,
!        ;      VREM(LCIK),ARPO
            WRITE (*,*) LCI,N,LM,ITER,ARP,DE,B,P,IB,JN,IKO,JKO,PAR2,R1,
     ;      VREM(LCIK),ARPO
            IF(KPOVT*(LCI/KPOVT).LT.LCI) GOTO 214
            WRITE (*,*) "WAIT"
            READ 219, J
  219       FORMAT (I1)
            IF(J.EQ.7) GOTO 213
            IF(J.EQ.4) STOP
            IF(J.LE.0) GOTO 135
  214       IF(LCI.GE.2) GOTO 215
  213       IF(JN.LT.12) JN=21
            CALL PECH(JKO, JN,JSUX)
            ! WRITE (*,45)
            WRITE (*,*) LCI,N,LM,ITER,ARP,DE,B,P,IB,JN,IKO,JKO,PAR2,R1,
     ;      VREM(LCIK),ARPO
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  215       IF(JSUX.LE.0) GOTO 139
            CALL ENERO(MAT,ARPO,KX,KZ)
  139    CONTINUE
   ! 46    FORMAT (4I4,F11.7,2F11.4,F8.1,4I5,F5.1,F10.6,F6.1,F8.5)
  135    IF(JN.LT.12) JN=21
         CALL PECH(JKO,JN,JSUX)
         IB=1+LCIK/13
         IF(13*(LCIK/13).EQ.LCIK) IB=IB-1
         ! WRITE(8,140)
         WRITE(*,9) (VREM(J),J=1,LCIK,IB),VREM(LCIK)
    9    FORMAT (7X,14F8.1)
         DO 142 J=1,KZ
            WRITE (*,141) Z(J),(SUT(J,I),I=1,LCIK,IB),SUT(J,LCIK)
  142    CONTINUE
         ! WRITE(8,146)
         WRITE(*,9) (VREM(J),J=1,LCIK,IB),VREM(LCIK)
         DO 149 J=1,KZ
            WRITE (*,141) Z(J),(SUX(J,I),I=1,LCIK,IB),SUX(J,LCIK)
  149    CONTINUE
  ! 140    FORMAT (' []   ')
  ! 146    FORMAT (' []   ',
  !    ;   ' ')
  141    FORMAT (F6.3,14F8.1)
         STOP
      END PROGRAM PROGNOZ
      SUBROUTINE PECH(JKO,JN,JSUX)
         DIMENSION U(51,61,9),W(51,61,9),SOST(4,61),X(51),Z(61)
         DIMENSION G(51,61),SINT(51,61)
         COMMON /UWM/U,W/RAZM/X,Z,B/RABOT/SOST,ARP
         COMMON /GG/S0,G0,STEKO,G,SINT
         COMMON /NXZ/NX1,NX,KX,NZ1,NZ,KZ,N,N1
   33    FORMAT (5X,19F6.2/(F5.2,19F6.1))
   42    FORMAT (5X,19F6.2/(F5.2,19F6.0))
  119    FORMAT (F5.2,1X,4F14.6)
         NO=JN-9
         JK=JN+6
         IF(JK.GT.NZ1) JK=NZ1
         WRITE (*,*) "BEGIN GRAPH"
         WRITE (*,42) (X(I),I=1,19),(Z(J),(G(I,J),I=1,19),J=1,12)
         ! WRITE(*,*) "//"
         WRITE (*,42) (X(I),I=1,19),(Z(J),(G(I,J),I=1,19),J=NO,JK)
         WRITE(*,*) "//"
         WRITE (*,42) (X(I),I=1,37,2),(Z(J),(U(I,J,3),I=1,37,2),
     ;   J=1,12)
         ! WRITE(*,*) "//"
         WRITE (*,42) (X(I),I=1,37,2),(Z(J),(U(I,J,3),I=1,37,2),
     ;   J=NO,JK)
         WRITE(*,*) "//"
         WRITE (*,42) (X(I),I=1,37,2),(Z(J),(U(I,J,3),I=1,37,2),
     ;   J=1,12)
         ! WRITE(*,*) "//"
         WRITE (*,42) (X(I),I=1,37,2),(Z(J),(U(I,J,3),I=1,37,2),
     ;   J=NO,JK)
         WRITE(*,*) "//"
         WRITE (*,42) (X(I),I=1,37,2),(Z(J),(U(I,J,4),I=1,37,2),
     ;   J=1,12)
         ! WRITE(*,*) "//"
         WRITE (*,42) (X(I),I=1,37,2),(Z(J),(U(I,J,4),I=1,37,2),
     ;   J=NO,JK)
         WRITE(*,*) "//"
         WRITE (*,42) (X(I),I=1,37,2),(Z(J),(U(I,J,4),I=1,37,2),
     ;   J=1,12)
         ! WRITE(*,*) "//"
         WRITE (*,42) (X(I),I=1,37,2),(Z(J),(U(I,J,4),I=1,37,2),
     ;   J=NO,JK)
         WRITE(*,*) "//"
         WRITE (*,42) (X(I),I=1,37,2),(Z(J),(SINT(I,J),I=1,37,2),
     ;   J=1,JK,2)
         WRITE(*,*) "//"
         IF(JSUX.EQ.1) GOTO 1
         WRITE (*,42) (X(I),I=1,37,2),(Z(J),(W(I,J,9),I=1,37,2),
     ;   J=1,JK,2)
    1    CONTINUE
         WRITE(*,*) "//"
         WRITE (*,33) (X(I),I=1,37,2),(Z(J),(U(I,J,7),I=1,37,2),
     ;   J=1,JKO)
         WRITE(*,*) "//"
         WRITE (*,33) (X(I),I=1,37,2),(Z(J),(W(I,J,7),I=1,37,2),
     ;   J=1,JKO)
         WRITE(*,*) "//"
         WRITE (*,33) (X(I),I=1,37,2),(Z(J),(W(I,J,8),I=1,37,2),
     ;   J=1,JKO)
         WRITE(*,*) "//"
         WRITE (*,33) (X(I),I=1,37,2),(Z(J),(U(I,J,8),I=1,37,2),
     ;   J=1,JKO)
         WRITE(*,*) "//"
         DO 118 J=1,JKO
            WRITE (*,119) Z(J),(SOST(I,J),I=1,4)
  118    CONTINUE
         RETURN
      END
      SUBROUTINE NOST(L,M,PAR1,PAR2,IKO,JKO,H)
         DIMENSION U(51,61,9),W(51,61,9),SOST(4,61),IK(61)
         COMMON /UWM/U,W/RAZM/X(51),Z(61),B/RABOT/SOST,ARP
         COMMON /NXZ/NX1,NX,KX,NZ1,NZ,KZ,N,N1
         PAR1=0.2
         PAR2=0.2
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         DEP=PAR2
         KX3=KX-3
         DO 1 J=1,KZ
    1    IK(J)=KX
   16    DO 15 I=3,KX,2
            IF(ABS(U(I,1,L)).GE.PAR1) GOTO 15 !¢ ®à¨£¨­ «¥ GE162R1
            IF(ABS(W(I,1,L)).GE.PAR1) GOTO 15 !¢¬¥áâ® GE.R1
            IF(ABS(U(I-1,1,L)).GE.PAR1) GOTO 15
            IF(ABS(W(I-1,1,L)).GE.PAR1) GOTO 15
            IK(1)=I
            IF(I.LT.KX3) GOTO 18
   15    CONTINUE
         PAR1=PAR1+DEP
         GOTO 16
   18    DO 2 JP=2,KZ
            IF(IK(JP).GE.KX) GOTO 3
    2    CONTINUE
    7    JP=2
    3    DO 4 J=JP,KZ
    6       DO 5 I=3,KX3,2
               IF(ABS(U(I,J,L)).GE.PAR2) GOTO 5 !âãâ GE162R2
               IF(ABS(W(I,J,L)).GE.PAR2) GOTO 5
               IF(ABS(U(I-1,J,L)).GE.PAR2) GOTO 5
               IF(ABS(W(I-1,J,L)).GE.PAR2) GOTO 5
               IK(J)=I
               IF(I.EQ.3) GOTO 8
               GOTO 4
    5       CONTINUE
            PAR2=PAR2+DEP
            GOTO 6
    4    CONTINUE
         PAR2=PAR2+DEP
         GOTO 7
    8    JKO=1
   13    JKO=JKO+2
         IF(JKO.LT.J) GOTO 13
         IF(JKO.GT.J) IK(JKO)=3
         R=B*H/3.0
         DO 9 J=1,JKO
            IN=IK(J)
            R1=-X(IN)*U(IN,J,L)/2.0
            R2=-X(IN)*W(IN,J,L)/2.0
            R3=-X(IN)*U(IN,J,M)/2.0
            R4=-X(IN)*W(IN,J,M)/2.0
            DO 10 I=3,IN,2
               IM=I-1
               R1=R1+2.0*X(IM)*U(IM,J,L)+X(I)*U(I,J,L)
               R2=R2+2.0*X(IM)*W(IM,J,L)+X(I)*W(I,J,L)
               R3=R3+2.0*X(IM)*U(IM,J,M)+X(I)*U(I,J,M)
   10       R4=R4+2.0*X(IM)*W(IM,J,M)+X(I)*W(I,J,M)
            SOST(1,J)=R*R1
            SOST(2,J)=R*R2
            SOST(3,J)=R*R3
    9    SOST(4,J)=R*R4
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         IKO=1
         DO 11 J=2,JKO
            IF(IKO.GE.IK(J)) GOTO 11
            IKO=IK(J)
   11    CONTINUE
         RETURN
      END
      SUBROUTINE PERE(K,P,V,G0)
         DIMENSION U(51,61,9),W(51,61,9)
         COMMON /UWM/U,W/RAZM/X(51),Z(61),B
         COMMON /NXZ/NX1,NX,KX,NZ1,NZ,KZ,N,N1
         GB=P/G0/B
         W(1,1,K)=3.0*GB*(1.0-V)/8.0
         U(1,1,K)=0.0
         C=0.2387324*GB
         V1=1.0-V
         V2=1.0-2.0*V
         DO 1 J=2,NZ1
            U(1,J,K)=0.0
    1    W(1,J,K)=C*((V1-V*Z(J)**2)*ATAN(1.0/Z(J))+V*Z(J))
         C1=0.07958*GB*V2
         C2=0.1875*V1*GB
         DO 2 I=2,N
            R=1.0-X(I)**2
            U(I,1,K)=C1*(R*SQRT(R)-1.0)/X(I)
    2    W(I,1,K)=C2*(1.0+R)
         C2=C*V1/2.0
         DO 3 I=N1,NX1
            R=X(I)**2-1.0
            U(I,1,K)=-C1/X(I)
    3    W(I,1,K)=C2*((1.0-R)*ASIN(1.0/X(I))+SQRT(R))
         DO 4 J=2,NZ1
            Z2=Z(J)**2
            DO 4 I=2,NX1
               X2=X(I)**2
               C1=X2+Z2-1.0
               C2=SQRT(C1*C1+4.0*Z2)
               R=C1+C2
               R1=SQRT(R/2.0)
               R2=ATAN(1.0/R1)
               W(I,J,K)=C*(R2*(V1*(2.0-X2)-2.0*V*Z2)+V1*R1-
     ;         (V2-V)*Z2/R1)/2.0
               R2=R2*V1*X(I)*Z(J)
               R=(R1*(7.0*C2+9.0*C1+6.0*Z2)+ 3.0*Z(J)*(3.*R-2.))/
     ;         (12.*R*R*X(I)*C2)
               R2=R2-V2*R*(R1-Z(J))**3
    4    U(I,J,K)=C*(R2-Z(J)*(X2*V2*(7.0*C2+C1+2.0)+4.0*C2*
     ;   (C1+C2-2.0*Z2))
     ;   /(16.0*X(I)*C2*R1))
         RETURN
      END
      SUBROUTINE MODG(EG,NX1,NZ1,L,M,MAT)
         DIMENSION U(51,61,9),W(51,61,9),SINT(51,61),G(51,61)
         DIMENSION SUX(61,42),SUT(61,42)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         COMMON /UWM/U,W/GG/S0,G0,STEKO,G,SINT/SUXLCI/SUX,SUT,LCI,JSUX
         EG=0.0
         RS=1.0/S0
         IF(MAT.EQ.2) GOTO 40
         IF(MAT.EQ.3) GOTO 50
C                        N1
         DO 21 J=1,NZ1
            COEF=SUT(J,LCI)/STEKO
            DO 21 I=1,NX1
               R1=G(I,J)
               RT=U(I,J,L)+SUX(J,LCI)
               RO=U(I,J,M)+SUX(J,LCI)
               SI=(RT-RO)**2+(RT-W(I,J,L))**2+(RO-W(I,J,L))**2+6.0*
     ;         W(I,J,M)**2
C              SI=(U(I,J,L)-U(I,J,M))**2+(U(I,J,L)-W(I,J,L))**2+(U(I,J,M)-
C    ;         W(I,J,L))**2+6.0*W(I,J,M))**2
               SI=SQRT(SI/2.0)/COEF
               EI=SI*(RS+1.0/G(I,J))/3.0
               IF(EI.GT.0.008) GOTO 12
               G(I,J)=G0
               GOTO 21
   12          IF(EI.GT.0.01) GOTO 13
               SI=108.0+6000.0*EI
               GOTO 18
   13          IF(EI.GT.0.014) GOTO 14
               SI=128.0+4000.0*EI
               GOTO 18
   14          IF(EI.GT.0.022) GOTO 15
               SI=135.0+3500.0*EI
               GOTO 18
   15          IF(EI.GT.0.027) GOTO 17
               SI=106.4+4800.0*EI
               GOTO 18
   17          SI=60.5+6500.0*EI
   18          G(I,J)=1.0/(3.0*EI/SI-RS)
               EPO=ABS(R1-G(I,J))
               IF(EPO.GT.EG) EG=EPO
   21    SINT(I,J)=SI*COEF
         RETURN
C                        N2
   40    DO 41 J=1,NZ1
            COEF=SUT(J,LCI)/STEKO
            DO 41 I=1,NX1
               R1=G(I,J)
               RT=U(I,J,L)+SUX(J,LCI)
               RO=U(I,J,M)+SUX(J,LCI)
               SI=(RT-RO)**2+(RT-W(I,J,L))**2+(RO-
     ;         W(I,J,L))**2+6.0*W(I,J,M)**2
C              SI=(U(I,J,L)-U(I,J,M))**2+(U(I,J,L)-W(I,J,L))**2+(U(I,J,M)-
C    ;         W(I,J,L))**2+6.0*W(I,J,M))**2
               SI=SQRT(SI/2.0)/COEF
               EI=SI*(RS+1.0/G(I,J))/3.0
               IF(EI.GT.0.008) GOTO 32
               G(I,J)=G0
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
               GOTO 41
   32          IF(EI.GT.0.01) GOTO 33
               SI=116.0+5000.0*EI
               GOTO 38
   33          IF(EI.GT.0.014) GOTO 34
               SI=141.0+2500.0*EI
               GOTO 38
   34          IF(EI.GT.0.024) GOTO 35
               SI=153.6+1600.0*EI
               GOTO 38
   35          IF(EI.GT.0.028) GOTO 36
               SI=144.0+2000.0*EI
               GOTO 38
   36          SI=18.0+6500.0*EI
   38          G(I,J)=1.0/(3.0*EI/SI-RS)
               EPO=ABS(R1-G(I,J))
               IF(EPO.GT.EG) EG=EPO
   41    SINT(I,J)=SI*COEF
         RETURN
C                      N3
   50    DO 51 J=1,NZ1
            COEF=SUT(J,LCI)/STEKO
            DO 51 I=1,NX1
               R1=G(I,J)
               RT=U(I,J,L)+SUX(J,LCI)
               RO=U(I,J,M)+SUX(J,LCI)
               SI=(RT-RO)**2+(RT-W(I,J,L))**2+(RO-W(I,J,L))**2+6.0*
     ;         W(I,J,M)**2
C              SI=(U(I,J,L)-U(I,J,M))**2+(U(I,J,L)-W(I,J,L))**2+(U(I,J,M)-
C    ;         W(I,J,L))**2+6.0*W(I,J,M))**2
               SI=SQRT(SI/2.0)/COEF
               EI=SI*(RS+1.0/G(I,J))/3.0
               IF(EI.GT.0.008) GOTO 42
               G(I,J)=G0
               GOTO 51
   42          IF(EI.GT.0.01) GOTO 43
               SI=124.0+4000.0*EI
               GOTO 48
   43          IF(EI.GT.0.014) GOTO 44
               SI=151.5+1250.0*EI
               GOTO 48
C  44          IF(EI.GT.0.022) GOTO 45
   44          SI=155.0+1000.0*EI
C              GOTO 48
C  45          IF(EI.GT.0.026) GOTO 46
C              SI=138.5+1750.0*EI
C              GOTO 48
C  46          IF(EI.GT.0.030) GOTO 47
C              SI=80.0+4000.0*EI
C              GOTO 48
C  47          SI=89.0+3600.0*EI
   48          G(I,J)=1.0/(3.0*EI/SI-RS)
               EPO=ABS(R1-G(I,J))
               IF(EPO.GT.EG) EG=EPO
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   51    SINT(I,J)=SI*COEF
         RETURN
      END
      SUBROUTINE NAPR(K,L,M)
         DIMENSION U(51,61,9),W(51,61,9),SINT(51,61),G(51,61)
         DIMENSION X(51),Z(61),XR(51)
         COMMON /UWM/U,W/GG/S0,G0,STEKO,G,SINT/RAZM/X,Z,B/XRAB/XR,
     ;   H,PANAP
         COMMON /NXZ/NX1,NX,KX,NZ1,NZ,KZ,N,N1
         HA=H/2.0
         DO 1 J=1,NZ1
            U(1,J,L)=(4.0*U(2,J,K)-U(3,J,K))/H
            U(1,J,M)=U(1,J,L)
            U(NX1,J,L)=(U(KX,J,K)-4.0*U(NX,J,K)+3.0*U(NX1,J,K))/H
            U(NX1,J,M)=U(NX1,J,K)/XR(NX1)
    1    W(1,J,M)=0.0
         DO 2 J=2,NZ
            JP=J+1
            JM=J-1
            W(NX1,J,M)=G(NX1,J)*(U(NX1,JP,K)-U(NX1,JM,K)+W(KX,J,K)-4.0*
     ;      W(NX,J,K)+3.0*W(NX1,J,K))/H
            W(1,J,L)=(W(1,JP,K)-W(1,JM,K))/H
    2    W(NX1,J,L)=(W(NX1,JP,K)-W(NX1,JM,K))/H
         W(1,1,L)=(-3.0*W(1,1,K)+4.0*W(1,2,K)-W(1,3,K))/H
C        W(1,NZ1,L)=(3.0*W(1,NZ1,K)-4.0*W(1,NZ,K)+W(1,KZ,K))/H
         W(1,NZ1,L)=(W(1,NZ1,K)-W(1,NZ,K))/HA
         W(NX1,NZ1,L)=(W(NX1,NZ1,K)-W(NX1,NZ,K))/HA
         W(NX1,1,M)=0.0
         W(NX1,NZ1,M)=G(NX1,NZ1)*(U(NX1,NZ1,K)-U(NX1,NZ,K)+W(NX1,
     ;   NZ1,K)
     ;   -W(NX,NZ1,K))/HA
         DO 4 I=2,NX
            IP=I+1
            IM=I-1
C           U(I,1,L)=(U(IP,1,K)-U(IM,1,K))/H
            U(I,NZ1,L)=(U(IP,NZ1,K)-U(IM,NZ1,K))/H
            U(I,NZ1,M)=U(I,NZ1,K)/XR(I)
            W(I,NZ1,L)=(3.0*W(I,NZ1,K)-4.0*W(I,NZ,K)+W(I,KZ,K))/H
            W(I,1,M)=0.0
            W(I,NZ1,M)=(3.0*U(I,NZ1,K)-4.0*U(I,NZ,K)+U(I,KZ,K)+
     ;      W(IP,NZ1,K)
     ;      -W(IM,NZ1,K))*G(I,NZ1)/H
            DO 4 J=2,NZ
               U(I,J,L)=(U(IP,J,K)-U(IM,J,K))/H
               W(I,J,L)=(W(I,J+1,K)-W(I,J-1,K))/H
               U(I,J,M)=U(I,J,K)/XR(I)
    4    W(I,J,M)=G(I,J)*(U(I,J+1,K)-U(I,J-1,K)+W(IP,J,K)-
     ;   W(IM,J,K))/H
         DO 5 J=2,NZ1
            DO 5 I=1,NX1
               R1=2.0*G(I,J)
               R2=(U(I,J,L)+U(I,J,M)+W(I,J,L))*(S0-R1)/3.0
               U(I,J,L)=R2+R1*U(I,J,L)
               W(I,J,L)=R2+R1*W(I,J,L)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    5    U(I,J,M)=R2+R1*U(I,J,M)
         DO 7 I=N1,NX1
            U(I,1,M)=2.0*G(I,1)*U(I,1,K)/XR(I)
            U(I,1,L)=-U(I,1,M)
    7    W(I,1,L)=0.0
         DO 8 I=2,N
            X2=X(I)**2
            R1=SQRT(1.0-X2)
            R2=(R1**3-1.0)/X2/3.0
            ET=PANAP*R2
            ER=-PANAP*(R1+R2)
            EZ=(-3.0*W(I,1,K)+4.0*W(I,2,K)-W(I,3,K))/H
            R1=2.0*G(I,1)
            R2=(ET+ER+EZ)*(S0-R1)/3.0
            U(I,1,L)=R2+R1*ER
            W(I,1,L)=R2+R1*EZ
    8    U(I,1,M)=R2+R1*ET
         ET=-PANAP/2.0
         W(1,1,L)=(-3.0*W(1,1,K)+4.0*W(1,2,K)-W(1,3,K))/H
         R1=2.0*G(1,1)
         R2=(2.0*ET+W(1,1,L))*(S0-R1)/3.0
         U(1,1,L)=R2+R1*ET
         W(1,1,L)=R2+R1*W(1,1,L)
         U(1,1,M)=R2+R1*ET
         RETURN
      END
      SUBROUTINE ENER(MAT,L,M,KX,KZ,ARP)
         DIMENSION U(51,61,9),W(51,61,9),G(51,61),SINT(51,61)
         DIMENSION SUX(61,42),SUT(61,42),XR(51),EN(61)
         COMMON /UWM/U,W/GG/S0,G0,STEKO,G,SINT/SUXLCI/SUX,SUT,LCI,
     ;   JSUX
         COMMON /XRAB/XR,H,PANAP
         RS=1.0/S0
         RSE=1.5/S0
         IF(MAT.EQ.2) GOTO 40
         IF(MAT.EQ.3) GOTO 50
C                        N1
         DO 21 J=1,KZ
            COEF=SUT(J,LCI)/STEKO
            VRA=SUX(J,LCI)*JSUX
            DO 21 I=1,KX
               RT=U(I,J,L)+VRA
               RO=U(I,J,M)+VRA
               SI=(RT-RO)**2+(RT-W(I,J,L))**2+(RO-W(I,J,L))**2+6.0*
     ;         W(I,J,M)**2
C              SI=(U(I,J,L)-U(I,J,M))**2+(U(I,J,L)-W(I,J,L))**2+(U(I,J,M)-
C    ;         W(I,J,L))**2+6.0*W(I,J,M))**2
               SI=SQRT(SI/2.0)/COEF
               EI=SI*(RS+1.0/G(I,J))/3.0
               IF(EI.GT.0.008) GOTO 12
               R=EI*SI/2.0
               GOTO 18
   12          IF(EI.GT.0.01) GOTO 13
               R=0.624+0.5*(SI+156.0)*(EI-0.008)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
               GOTO 18
   13          IF(EI.GT.0.014) GOTO 14
               R=0.948+0.5*(SI+168.0)*(EI-0.01)
               GOTO 18
   14          IF(EI.GT.0.022) GOTO 15
               R=1.652+0.5*(SI+184.0)*(EI-0.014)
               GOTO 18
   15          IF(EI.GT.0.027) GOTO 17
               R=3.236+0.5*(SI+212.0)*(EI-0.022)
               GOTO 18
   17          R=4.356+0.5*(SI+236.0)*(EI-0.027)
   18          W(I,J,9)=SI*COEF
               U(I,J,9)=R*COEF**2+RSE*(U(I,J,L)+W(I,J,L)+U(I,J,M))**2
   21    CONTINUE
         GOTO 73
C                         N2
   40    DO 31 J=1,KZ
            COEF=SUT(J,LCI)/STEKO
            VRA=SUX(J,LCI)*JSUX
            DO 31 I=1,KX
               RT=U(I,J,L)+VRA
               RO=U(I,J,M)+VRA
               SI=(RT-RO)**2+(RT-W(I,J,L))**2+(RO-W(I,J,L))**2+6.0*
     ;         W(I,J,M)**2
C              SI=(U(I,J,L)-U(I,J,M))**2+(U(I,J,L)-W(I,J,L))**2+(U(I,J,M)-
C    ;         W(I,J,L))**2+6.0*W(I,J,M))**2
               SI=SQRT(SI/2.0)/COEF
               EI=SI*(RS+1.0/G(I,J))/3.0
               IF(EI.GT.0.008) GOTO 32
               R=EI*SI/2.0
               GOTO 38
   32          IF(EI.GT.0.01) GOTO 33
               R=0.624+0.5*(SI+156.0)*(EI-0.008)
               GOTO 38
   33          IF(EI.GT.0.014) GOTO 34
               R=0.946+0.5*(SI+166.0)*(EI-0.010)
               GOTO 38
   34          IF(EI.GT.0.024) GOTO 35
               R=1.630+0.5*(SI+176.0)*(EI-0.014)
               GOTO 38
   35          IF(EI.GT.0.028) GOTO 36
               R=3.470+0.5*(SI+192.0)*(EI-0.024)
               GOTO 38
   36          R=4.254+0.5*(SI+200.0)*(EI-0.028)
   38          W(I,J,9)=SI*COEF
               U(I,J,9)=R*COEF**2+RSE*(U(I,J,L)+W(I,J,L)+U(I,J,M))**2
   31    CONTINUE
         GOTO 73
C                         N3
   50    DO 51 J=1,KZ
            COEF=SUT(J,LCI)/STEKO
            VRA=SUX(J,LCI)*JSUX
            DO 51 I=1,KX
               RT=U(I,J,L)+VRA
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
               RO=U(I,J,M)+VRA
               SI=(RT-RO)**2+(RT-W(I,J,L))**2+(RO-W(I,J,L))**2+6.0*
     ;         W(I,J,M)**2
C              SI=(U(I,J,L)-U(I,J,M))**2+(U(I,J,L)-W(I,J,L))**2+(U(I,J,M)-
C    ;         W(I,J,L))**2+6.0*W(I,J,M))**2
               SI=SQRT(SI/2.0)/COEF
               EI=SI*(RS+1.0/G(I,J))/3.0
               IF(EI.GT.0.008) GOTO 42
               R=EI*SI/2.0
               GOTO 48
   42          IF(EI.GT.0.01) GOTO 43
               R=0.624+0.5*(SI+156.0)*(EI-0.008)
               GOTO 48
   43          IF(EI.GT.0.014) GOTO 44
               R=0.944+0.5*(SI+164.0)*(EI-0.010)
               GOTO 48
C  44          IF(EI.GT.0.022) GOTO 45
   44          R=1.610+0.5*(SI+169.0)*(EI-0.014)
C              GOTO 48
C  45          IF(EI.GT.0.026) GOTO 46
C              R=2.994+0.5*(SI+177.0)*(EI-0.022)
C              GOTO 48
C  46          IF(EI.GT.0.030) GOTO 47
C              R=3.716+0.5*(SI+177.0)*(EI-0.022)
C              GOTO 48
C  47          R=4.484+(SI+200.0)*(EI-0.030)
   48          W(I,J,9)=SI*COEF
               U(I,J,9)=R*COEF**2+RSE*(U(I,J,L)+W(I,J,L)+U(I,J,M))**2
   51    CONTINUE
   73    R=3.14159*H/1.5
         DO 115 J=1,KZ
            R2=-XR(KX)*U(KX,J,9)/2.0
            DO 74 I=3,KX,2
   74       R2=R2+2.0*XR(I-1)*U(I-1,J,9)+XR(I)*U(I,J,9)
  115    EN(J)=R2*R
         ARP=(EN(1)-EN(KZ))/2.0
         DO 117 J=2,KZ,2
  117    ARP=ARP+EN(J+1)+2.0*EN(J)
         ARP=9.81E-03*H*ARP/3.0
         RETURN
      END
      SUBROUTINE ENERO(MAT,ARPO,KX,KZ)
         DIMENSION EN(61),UEN(51),G(51,61),SINT(51,61)
         DIMENSION SUX(61,42),SUT(61,42),XR(51)
         COMMON /GG/S0,G0,STEKO,G,SINT/SUXLCI/SUX,SUT,LCI,JSUX
         COMMON /XRAB/XR,H,PANAP
         RS=1.0/S0
         RH=3.14159*H/1.5
         RK=3.14159*XR(KX)**2*6.0/S0
         IND=LCI+1
         IF(MAT.EQ.2) GOTO 40
         IF(MAT.EQ.3) GOTO 50
C                        N1
         DO 21 J=1,KZ
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            COEF=SUT(J,IND)/STEKO
            SI=ABS(SUX(J,IND))/COEF
            DO 18 I=2,KX
               EI=SI*(RS+1.0/G(I,J))/3.0
               IF(EI.GT.0.008) GOTO 12
               R=EI*SI/2.0
               GOTO 18
   12          IF(EI.GT.0.01) GOTO 13
               R=0.624+0.5*(SI+156.0)*(EI-0.008)
               GOTO 18
   13          IF(EI.GT.0.014) GOTO 14
               R=0.948+0.5*(SI+168.0)*(EI-0.01)
               GOTO 18
   14          IF(EI.GT.0.022) GOTO 15
               R=1.652+0.5*(SI+184.0)*(EI-0.014)
               GOTO 18
   15          IF(EI.GT.0.027) GOTO 17
               R=3.236+0.5*(SI+212.0)*(EI-0.022)
               GOTO 18
   17          R=4.356+0.5*(SI+236.0)*(EI-0.027)
   18       UEN(I)=R
            R2=-XR(KX)*UEN(KX)/2.0
            DO 19 I=3,KX,2
   19       R2=R2+2.0*XR(I-1)*UEN(I-1)+XR(I)*UEN(I)
            EN(J)=R2*RH*COEF**2+RK*SUX(J,IND)**2
   21    CONTINUE
         GOTO 73
C                        N2
   40    DO 31 J=1,KZ
            COEF=SUT(J,IND)/STEKO
            SI=ABS(SUX(J,IND))/COEF
            DO 38 I=2,KX
               SI=SQRT(SI/2.0)/COEF
               EI=SI*(RS+1.0/G(I,J))/3.0
               IF(EI.GT.0.008) GOTO 32
               R=EI*SI/2.0
               GOTO 38
   32          IF(EI.GT.0.01) GOTO 33
               R=0.624+0.5*(SI+156.0)*(EI-0.008)
               GOTO 38
   33          IF(EI.GT.0.014) GOTO 34
               R=0.946+0.5*(SI+166.0)*(EI-0.010)
               GOTO 38
   34          IF(EI.GT.0.024) GOTO 35
               R=1.630+0.5*(SI+176.0)*(EI-0.014)
               GOTO 38
   35          IF(EI.GT.0.028) GOTO 36
               R=3.470+0.5*(SI+192.0)*(EI-0.024)
               GOTO 38
   36          R=4.254+0.5*(SI+200.0)*(EI-0.028)
   38       UEN(I)=R
            R2=-XR(KX)*UEN(KX)/2.0
            DO 39 I=3,KX,2
   39       R2=R2+2.0*XR(I-1)*UEN(I-1)+XR(I)*UEN(I)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            EN(J)=R2*RH*COEF**2+RK*SUX(J,IND)**2
   31    CONTINUE
         GOTO 73
C                        N3
   50    DO 51 J=1,KZ
            COEF=SUT(J,IND)/STEKO
            SI=ABS(SUX(J,IND))/COEF
            DO 48 I=2,KX
               EI=SI*(RS+1.0/G(I,J))/3.0
               IF(EI.GT.0.008) GOTO 42
               R=EI*SI/2.0
               GOTO 48
   42          IF(EI.GT.0.01) GOTO 43
               R=0.624+0.5*(SI+156.0)*(EI-0.008)
               GOTO 48
   43          IF(EI.GT.0.014) GOTO 44
               R=0.944+0.5*(SI+164.0)*(EI-0.010)
               GOTO 48
C  44          IF(EI.GT.0.022) GOTO 45
   44          R=1.610+0.5*(SI+169.0)*(EI-0.014)
C              GOTO 48
C  45          IF(EI.GT.0.026) GOTO 46
C              R=2.994+0.5*(SI+177.0)*(EI-0.022)
C              GOTO 48
C  46          IF(EI.GT.0.30) GOTO 47
C              R=3.716+0.5*(SI+184.0)*(EI-0.026)
C              GOTO 48
C  47          R=4.484+(SI+200.0)*(EI-0.030)
   48       UEN(I)=R
            R2=-XR(KX)*UEN(KX)/2.0
            DO 49 I=3,KX,2
   49       R2=R2+2.0*XR(I-1)*UEN(I-1)+XR(I)*UEN(I)
            EN(J)=R2*RH*COEF**2+RK*SUX(J,IND)**2
   51    CONTINUE
   73    ARPO=(EN(1)-EN(KZ))/2.0
         DO 117 J=2,KZ,2
  117    ARPO=ARPO+EN(J+1)+2.0*EN(J)
         ARPO=ARPO*9.81E-03*H/3.0
         RETURN
      END
      SUBROUTINE ROK(L,M,KR)
         DIMENSION U(51,61,9),W(51,61,9),XR(51)
         COMMON /UWM/U,W/XRAB/XR,H,PANAP
         COMMON /NXZ/NX1,NX,KX,NZ1,NZ,KZ,N,N1
         HA=H/2.0
         NRA=N1+1
         DO 1 I=2,NX
            IP=I+1
            IM=I-1
            DO 1 J=2,NZ
               U(I,J,KR)=(U(IP,J,L)-U(IM,J,L)+W(I,J+1,M)-W(I,J-1,M))/H+
     ;         (U(I,J,L)
     \         -U(I,J,M))/XR(I)
    1    W(I,J,KR)=(W(IP,J,M)-W(IM,J,M)+W(I,J+1,L)-W(I,J-1,L))/H+
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     ;   W(I,J,M)
     \   /XR(I)
         DO 2 I=1,N
            U(I,1,KR)=0.0
    2    W(I,1,KR)=0.0
         DO 3 I=NRA,NX
            U(I,1,KR)=(U(I+1,1,L)-U(I-1,1,L)+2.0*W(I,2,M))/H+
     ]      (U(I,1,L)-U(I,1,M))/XR(I)
    3    W(I,1,KR)=(W(I,2,L)-W(I,1,L))/HA
         U(N1,1,KR)=(4.0*U(NRA,1,L)-U(N1+1,1,L)-3.0*U(N1,1,L)+2.0*
     ;   W(I,2,M))
     \   /H+(U(N1,1,L)-U(N1,1,M))/XR(N1)
         W(N1,1,KR)=(W(N1,2,L)-W(N1,1,L))/HA
         DO 4 J=2,NZ
            U(1,J,KR)=0.0
            U(NX1,J,KR)=(-2.0*U(NX,J,L)+W(NX1,J+1,M)-W(NX1,J-1,M))/H-
     \      U(NX1,J,M)/XR(NX1)
            W(1,J,KR)=(4.0*W(2,J,M)+W(1,J+1,L)-W(1,J-1,L))/H
            W(NX1,J,KR)=(W(NX1,J+1,L)-W(NX1,J-1,L)-2.0*W(NX,J,M))/H
    4    CONTINUE
         DO 5 I=2,NX
            U(I,NZ1,KR)=(U(I+1,NZ1,L)-U(I-1,NZ1,L)-2.0*W(I,NZ,M))/H+
     \      (U(I,NZ1,L)-U(I,NZ1,M))/XR(I)
            W(I,NZ1,KR)=(W(I+1,NZ1,M)-W(I-1,NZ1,M)-2.0*W(I,NZ,L))/H+
     \      W(I,NZ1,M)/XR(I)
    5    CONTINUE
         U(NX1,1,KR)=(W(NX1,2,M)-U(NX,1,L))/HA-U(NX1,1,M)/XR(NX1)
         U(NX1,NZ1,KR)=-(W(NX1,NZ,M)+U(NX,NZ1,L))/HA-U(NX1,NZ1,M)/
     ;   XR(NX1)
         U(1,NZ1,KR)=0.0
         W(NX1,1,KR)=(W(NX1,2,L)-W(NX1,1,L))/HA
         W(NX1,NZ1,KR)=-(W(NX,NZ1,M)+W(KX,NZ1,M))/HA
         W(1,NZ1,KR)=(2.0*W(2,NZ1,M)-W(1,NZ,L))/HA
         RETURN
      END
      SUBROUTINE PDV(IND)
         DIMENSION U(51,61,9),W(51,61,9),FB(61,2),FN(51,2)
         COMMON /UWM/U,W/SILA/FB,FN/A7/A,B,C,D,E,F,FST
         COMMON /NXZ/NX1,NX,KX,NZ1,NZ,KZ,N,N1
         FST=F
         A=0.0
         B=0.0
         C=0.0
         D=0.0
         E=0.0
         F=0.0
         CALL ROK(3,4,2)
         DO 1 I=1,NX1
            U(1,NZ1,2)=U(I,NZ1,2)+FN(I,1)
    1    W(1,NZ1,2)=W(I,NZ1,2)+FN(I,2)
         DO 2 J=1,NZ1
            U(NX1,J,2)=U(NX1,J,2)+FB(J,1)
    2    W(NX1,J,2)=W(NX1,J,2)+FB(J,2)
         CALL NAPR(2,3,4)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         CALL ROK(3,4,9)
         DO 3 I=1,NX1
            DO 3 J=1,NZ1
               A=A+U(I,J,9)*U(I,J,2)+W(I,J,9)*W(I,J,2)
               E=E+U(I,J,9)**2+W(I,J,9)**2
    3    F=F+U(I,J,2)**2+W(I,J,2)**2
         IF(IND.EQ.2) GOTO 7
         R=A/E
         DO 8 J=1,NZ1
            DO 8 I=1,NX1
               U(I,J,8)=U(I,J,2)
               W(I,J,8)=W(I,J,2)
               U(I,J,7)=U(I,J,1)
               W(I,J,7)=W(I,J,1)
               U(I,J,1)=U(I,J,1)-R*U(I,J,2)
    8    W(I,J,1)=W(I,J,1)-R*W(I,J,2)
         RETURN
    7    DO 4 I=1,NX1
            DO 4 J=1,NZ1
               B=B+U(I,J,9)*U(I,J,8)+W(I,J,9)*W(I,J,8)
    4    D=D+U(I,J,8)*U(I,J,2)+W(I,J,8)*W(I,J,2)
         C=F-D
         D=D-FST
         R=(A-B)/E
         R1=C-D-R*(A-B)
         R3=0.1
         IF(ABS(R1).GT.1.0E-08) R3=(R*B-D)/R1
         R=B/E+R3*R
         DO 14 I=1,NX1
            DO 14 J=1,NZ1
               R1=U(I,J,7)+R3*(U(I,J,1)-U(I,J,7))-R*U(I,J,2)
               R2=W(I,J,7)+R3*(W(I,J,1)-W(I,J,7))-R*W(I,J,2)
               U(I,J,8)=U(I,J,2)
               W(I,J,8)=W(I,J,2)
               U(I,J,7)=U(I,J,1)
               W(I,J,7)=W(I,J,1)
               U(I,J,1)=R1
   14    W(I,J,1)=R2
         RETURN
      END
      SUBROUTINE VSP(P,V,H,L,M)
         DIMENSION U(51,61,9),W(51,61,9),X(51),Z(61)
         DIMENSION FB(61,2),FN(51,2)
         COMMON /UWM/U,W/RAZM/X,Z,B/SILA/FB,FN
         COMMON /NXZ/NX1,NX,KX,NZ1,NZ,KZ,N,N1
         RO=-1.5*P/(3.14159*B*B)
         R2=SQRT(2.0)
         R8=8.0*R2
         R9=2.0*R2
         RS=RO*R2
         VV=(1.0-2.0*V)/48.0
         VP=V+1.0
         VOT=V/4.0/R2
         DO 1 I=2,NX1
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            W(I,1,M)=0.0
            X2=X(I)**2
            R1=X2-1.0
            RX=VV/X2
            DO 1 J=2,NZ1
               Z2=Z(J)**2
               GAM=Z2+R1
               DEL=SQRT(GAM*GAM+4.0*Z2)
               R4=GAM+DEL
               R3=SQRT(R4)
               RD=DEL*R3
               W(I,J,L)=2.0*RS*Z2*Z(J)/(RD*R4)
               W(I,J,M)=RS*Z2*(R4-2.0*Z2)/(X(I)*RD*R4)
               R5=R3*(7.0*DEL+9.0*GAM+6.0*Z2)+3.0*R2*Z(J)*(3.0*R4-2.0)
               R5=RX*R5*(R3-Z(J)*R2)**3/(DEL*R4*R4)
               RAT=VP*ATAN(R2/R3)
               R4=(R4-2.0*Z2)/(R9*X2*R3)
               R3=(7.0*DEL+GAM+2.0)/RD
               U(I,J,L)=RO*(Z(J)*((17.0*DEL+7.0*GAM-2.0)/(R8*RD)-R4-
     ;         RAT+VOT*R3)-R5)
               U(I,J,M)=RO*(R5+Z(J)*(VOT*(9.0*DEL-GAM-2.0)/RD-RAT+
     ;         R3/R8+R4))
    1    CONTINUE
         DO 2 J=2,NZ1
            R4=1.0/(1.0+Z(J)**2)
            W(1,J,L)=RO*R4
            W(1,J,M)=0.0
            U(1,J,L)=RO*(VP*(1.0-Z(J)*ATAN(1.0/Z(J)))-R4/2.0)
    2    U(1,J,M)=U(1,J,L)
         W(1,1,M)=0.0
         W(1,1,L)=RO
         U(1,1,L)=RO*(V+0.5)
         U(1,1,M)=RO*(V+0.5)
         DO 5 I=2,N
            X2=X(I)**2
            R5=SQRT(1.0-X2)
            R1=(1.0-2.0*V)*((1.0-X2)*R5-1.0)/3.0/X2
            U(I,1,L)=RO*(R5+R1)
            U(I,1,M)=RO*(2.0*V*R5-R1)
    5    W(I,1,L)=RO*R5
         R3=P*(0.5-V)/(3.14159*B*B)
         DO 6 I=N1,NX1
            W(I,1,L)=0.0
            U(I,1,L)=R3/X(I)**2
    6    U(I,1,M)=-U(I,1,L)
         R3=2.0/H
         R2=1.0/H+1.0/X(NX1)/B
         R1=2.0/H+1.0/X(NX1)/B
         DO 3 J=1,NZ1
            FB(J,2)=W(NX1,J,M)*R2
    3    FB(J,1)=U(NX1,J,L)*R1
         DO 4 I=1,NX1
            FN(I,1)=W(I,NZ1,M)*R3
    4    FN(I,2)=W(I,NZ1,L)*R3
         RETURN
      END
