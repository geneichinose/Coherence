C - FFT                                                                         
C                                                                               
C  Author:  Dave Harris                                                         
C           L-205                                                               
C           Lawrence Livermore National Laboratory                              
C           Livermore, CA  94550                                                
C                                                                               
C Last modified:  November 14, 1977                                             
C Program to compute the FFT of a sequence.                                     
C XREAL(*)         The real part of the sequence is stored in                   
C                  this array.                                                  
C XIMAG(*)         The imaginary part is stored here.                           
C TCOS(*),TSIN(*)  Cosine and sine tables.                                      
C N                The length of the sequence.                                  
C LOGN             The base-two logarithm of N.                                 
C IDIR             Variable to indicate the direction of the transform.         
C                  For a forward transform IDIR=-1 and for an inverse           
C                  transform IDIR=1.  Normalization is performed                
C                  for IDIR=1.                                                  
C The transform result is returned in XREAL and XIMAG.                          
C   The initial sequence is destroyed.                                          
      SUBROUTINE FFT(XREAL,XIMAG,N,IDIR)                                
      DIMENSION XREAL(1),XIMAG(1),TCOS(4097),TSIN(4097)                 
      DOUBLE PRECISION FUND,DCOSN,DSINE                                 
      NBY2=N/2                                                          
      LOGN=1                                                            
    1 CONTINUE                                                          
        IF (  2**LOGN .EQ. N ) THEN                                     
          GO TO    2                                                    
        ELSE                                                            
          LOGN=LOGN+1                                                   
        END IF                                                          
      GO TO    1                                                        
    2 CONTINUE                                                          
C                                                                               
C                                                                               
C     TABLE GENERATOR                                                           
C                                                                               
      FUND=3.141592653589793D0*2.0D0                                    
      FUND=FUND/FLOAT(N)                                                
      DCOSN=DCOS(FUND)                                                  
      DSINE=DSIN(FUND)                                                  
      IF (  IDIR.EQ.-1 ) THEN                                           
        DSINE=-DSINE                                                    
      END IF                                                            
      TCOS(1)=1.                                                        
      TSIN(1)=0.                                                        
      DO    3 I=2, NBY2                                                 
        TCOS(I)=DCOSN*TCOS(I-1)-DSINE*TSIN(I-1)                         
        TSIN(I)=DCOSN*TSIN(I-1)+DSINE*TCOS(I-1)                         
    3 CONTINUE                                                          
C                                                                               
C     BIT REVERSE CODE                                                          
C                                                                               
        NM1=N-1                                                         
        J=1                                                             
        DO    4 I=1, NM1                                                
        IF (  I.LT.J ) THEN                                             
          TEMP=XREAL(I)                                                 
          XREAL(I)=XREAL(J)                                             
          XREAL(J)=TEMP                                                 
          TEMP=XIMAG(I)                                                 
          XIMAG(I)=XIMAG(J)                                             
          XIMAG(J)=TEMP                                                 
        END IF                                                          
        K=NBY2                                                          
    5   CONTINUE                                                        
        IF (.NOT.( K.LT.J )) GO TO    6                                 
          J=J-K                                                         
          K=K/2                                                         
        GO TO    5                                                      
    6   CONTINUE                                                        
        J=J+K                                                           
    4   CONTINUE                                                        
C                                                                               
C     INDEXING CODE                                                             
C                                                                               
        NBLOCK=N                                                        
        IREL=1                                                          
        DO    7 NSTAGE=1, LOGN                                          
          IF (  NSTAGE.GT.1 ) THEN                                      
            IREL=IREL*2                                                 
          END IF                                                        
          I=-IREL                                                       
          NBLOCK=NBLOCK/2                                               
          DO    8 IBLOCK=1, NBLOCK                                      
            I=I+IREL                                                    
            ITI=1-NBLOCK                                                
            DO    9 ICOUNT=1, IREL                                      
              I=I+1                                                     
              ITI=ITI+NBLOCK                                            
              I1=I+IREL                                                 
C                                                                               
C             BUTTERFLY CODE                                                    
C                                                                               
              IF (  NSTAGE.GT.1 ) THEN                                  
                SINE=TSIN(ITI)                                          
                COSINE=TCOS(ITI)                                        
                TEMP=XREAL(I1)*COSINE-XIMAG(I1)*SINE                    
                XIMAG(I1)=XREAL(I1)*SINE+XIMAG(I1)*COSINE               
                XREAL(I1)=TEMP                                          
              END IF                                                    
              TEMP=XREAL(I)+XREAL(I1)                                   
              XREAL(I1)=XREAL(I)-XREAL(I1)                              
              XREAL(I)=TEMP                                             
              TEMP=XIMAG(I)+XIMAG(I1)                                   
              XIMAG(I1)=XIMAG(I)-XIMAG(I1)                              
              XIMAG(I)=TEMP                                             
C                                                                               
    9       CONTINUE                                                    
    8     CONTINUE                                                      
    7   CONTINUE                                                        
C                                                                               
C  IF REVERSE TRANSFORM, DIVIDE THROUGH BY N                                    
C                                                                               
        IF (  IDIR .EQ. 1 ) THEN                                        
          SCALE=1./FLOAT(N)                                             
          DO   10 I=1, N                                                
            XREAL(I)=XREAL(I)*SCALE                                     
            XIMAG(I)=XIMAG(I)*SCALE                                     
   10     CONTINUE                                                      
        END IF                                                          
C                                                                               
C  BYE                                                                          
C                                                                               
      RETURN                                                            
      END                                                               
