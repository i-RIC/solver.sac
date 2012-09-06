C=====CULOUT bof==============================================================
C
C-----Purpose:
C       this is a dummy version of the code for stand alone hydie op
C       or op with the slope-area code.  Dummied by putting C in col 1.
C       puts values into approac.inc from pout array. Called by hydie
C     Programmed by: JM Fulford
C     Date:
C     Modified by:
C     Last modified:
C
      SUBROUTINE CULOUT
     O                 (RD,XSNAME,INKT,NKTS,POUT)
C
C-----Arguments:
      INTEGER NKTS,INKT
      REAL RD,POUT(17)
      CHARACTER*16 XSNAME
C
C-----Module data:
C      INCLUDE 'APPROC.INC'
C
C      NQW=NKTS
C      APPID=XSNAME
C      APSRD=RD
C      I=INKT
CC      DO 10 I=1,NKTS
C        W1(I)=POUT(13)
C        A1(I)=POUT(3)
C        APH1(I)=POUT(9)
C        K1(I)=POUT(1)
C        B1(I)=POUT(2)
C        QC(I)=0.0
C        IF(A1(I).GT.0.0.AND.B1(I).GT.0.0)
C     #    QC(I)=A1(I)*SQRT(32.2*A1(I)/B1(I))
CC 10   CONTINUE
C
      WRITE(*,*)'Welcome to the dummy culout subroutine'
C
      RETURN
      END
C
C=====CULOUT eof==============================================================
C
