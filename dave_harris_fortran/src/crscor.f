C                                                             CRSCOR            
C  CRSCOR -- Subroutine to compute cross-correlation function                   
C                                                                               
C  Author:  Dave Harris                                                         
C           L-205                                                               
C           Lawrence Livermore National Laboratory                              
C           Livermore, Ca  94550                                                
C                                                                               
C  Input Arguments:                                                             
C  ----- ----------                                                             
C                                                                               
C    DATA1                  Array containing first sequence                     
C                                                                               
C    DATA2                  Array containing second sequence                    
C                                                                               
C    #SAMPLES               Number of samples in data                           
C                                                                               
C    #WINDOWS               Requested number of windows                         
C                                                                               
C    WINDOW_LENGTH          Requested number of samples in each window          
C                           the subroutine calculates window overlap.           
C                           Not to exceed 4096 in present configuration.        
C                                                                               
C    WINDOW_TYPE            Character*8 variable describing type of             
C                           data analysis window to be used.  Legal             
C                           types are:                                          
C                                                                               
C                                <HAM>MING                                      
C                                <HAN>NING                                      
C                                <C>OSINE                                       
C                                <R>ECTAN                                       
C                                <T>RIANG                                       
C                                                                               
C                                                                               
C  Output Arguments:                                                            
C  ------ ----------                                                            
C                                                                               
C    C                      Array containing resulting 2*WINDOW_LENGTH - 1      
C                           correlation coefficients.  The correlation          
C                           sequence is circularly rotated in the array,        
C                           rotated so that the zeroth lag is in C(0).          
C                           The array is dimensioned (0:8191).                  
C                                                                               
C    FFT_LENGTH             Number of samples in correlation sequence.          
C                           Sequence may be padded with zeroes.                 
C                                                                               
C    ERROR_MESSAGE          Error message, CHARACTER*130 variable.              
C                                                                               
C                                                                               
C                                                                               
C  Linkage:  FFT, ZERO, MOVE, WINDOW, RMS                                       
C                                                                               
C  Replacements                                                                 
C                                                                               
C                                                                               
C                                                                               
C  Author:  Dave Harris                                                         
C                                                                               
C  Created:  January 30, 1980                                                   
C                                                                               
C  Last Modified:  June 21, 1984                                                
C  expanded arrays jan 1988                                                     
C                                                                               
      SUBROUTINE CRSCOR(DATA1, DATA2, NSAMPS, NWIN, WLEN,               
     1  TYPE, C, NFFT, ERR)                                             
C                                                                               
        REAL*4 DATA1(1), DATA2(1)                                       
        REAL*4 C(0:8191), CAUX(0:8191)                                  
        REAL*4 W(0:4095)                                                
        REAL*4 WORKR(0:8191), WORKI(0:8191)                             
        CHARACTER*(*) ERR                                               
        CHARACTER*8 TYPE                                                
        INTEGER WLEN, HALF, NFFT, POINT                                 
C                                                                               
        COMMON /BIG/ CAUX, W, WORKR, WORKI                              
C                                                                               
C  Initializations                                                              
C                                                                               
        ERR = ' '                                                       
C                                                                               
C  Check for legal window length and compute overlap                            
C                                                                               
        NLAGS = 2*WLEN - 1                                              
        IF (  NWIN .LT. 1 ) THEN                                        
C                                                                               
          ERR = ' CRSCOR - too few windows '                            
          RETURN                                                        
C                                                                               
        ELSE IF ( WLEN .LT. 1 .OR. WLEN .GT. 4096 .OR. WLEN .GT. NSAMPS 
     1) THEN                                                            
C                                                                               
          ERR = ' CRSCOR - illegal window length '                      
          RETURN                                                        
C                                                                               
        ELSE                                                            
C                                                                               
C                                               Everything OK                   
          ERR = ' '                                                     
C                                                                               
          IF (  NWIN*WLEN .LE. NSAMPS ) THEN                            
            NVERLP = 0                                                  
          ELSE                                                          
            NVERLP = (NWIN*WLEN - NSAMPS)/(NWIN - 1)                    
            IF (  NWIN*WLEN - NVERLP*(NWIN-1) .GT. NSAMPS ) THEN        
              NVERLP = NVERLP + 1                                       
            END IF                                                      
          END IF                                                        
          LSAMP = WLEN - 1                                              
C                                                                               
        END IF                                                          
C                                                                               
C  Generate window                                                              
C                                                                               
        DO    1 I = 0, LSAMP                                            
          W(I) = 1.                                                     
C             I                                                                 
    1   CONTINUE                                                        
        CALL WINDOW(W(0), WLEN, TYPE, 1, WLEN,                          
     1              W(0), ERR)                                          
C                                                                               
C  Check validity of window calculation                                         
C                                                                               
        IF (  ERR .NE. ' ' ) THEN                                       
          RETURN                                                        
        END IF                                                          
C                                                                               
C  Find first power of two >= #LAGS                                             
C                                                                               
        NFFT = 8                                                        
    2   CONTINUE                                                        
        IF ( NFFT .GE. NLAGS ) GO TO    3                               
          NFFT = NFFT*2                                                 
        GO TO    2                                                      
    3   CONTINUE                                                        
        HALF = NFFT/2                                                   
C                                                                               
C  Compute cross-correlation function                                           
C                                                                               
C                                                                               
C    Initialize window pointer                                                  
C                                                                               
        POINT = 1                                                       
C                                                                               
C    Initialize correlation arrays                                              
C                                                                               
        CALL ZERO(C(0), 8192)                                           
        CALL ZERO(CAUX(0), 8192)                                        
C                                                                               
C    Compute cross-spectrum for each window,  then average                      
C                                                                               
        DO    4 I = 1, NWIN                                             
C                                                                               
C    Zero work arrays                                                           
C                                                                               
          CALL ZERO(WORKR(0), NFFT)                                     
          CALL ZERO(WORKI(0), NFFT)                                     
C                                                                               
C    Load data into arrays                                                      
C                                                                               
          CALL MOVE(WORKR(0), DATA1(POINT), WLEN)                       
          CALL MOVE(WORKI(0), DATA2(POINT), WLEN)                       
C                                                                               
C    Compute scale factors                                                      
C                                                                               
          SCALE1 = RMS(WORKR(0), WLEN)                                  
          SCALE2 = RMS(WORKI(0), WLEN)                                  
          SCALE = SCALE1*SCALE2                                         
C                                                                               
C    Window and scale data                                                      
C                                                                               
          DO    5 J = 0, LSAMP                                          
            WORKR(J) = WORKR(J)*W(J)/SCALE1                             
            WORKI(J) = WORKI(J)*W(J)/SCALE2                             
C               J                                                               
    5     CONTINUE                                                      
C                                                                               
C    Compute and average cross spectra                                          
C                                                                               
          CALL FFT(WORKR(0), WORKI(0), NFFT, -1)                        
C                                                                               
C      Special case for point at 0                                              
C                                                                               
          C(0) = C(0) + WORKR(0)*WORKI(0)*SCALE                         
C                                                                               
C      All other points                                                         
C                                                                               
          DO    6 J = 1, HALF                                           
C                                                                               
            K = NFFT - J                                                
C                                                                               
            XR = (WORKR(J) + WORKR(K))*.5                               
            XI = (WORKI(J) - WORKI(K))*.5                               
            YR = (WORKI(J) + WORKI(K))*.5                               
            YI = (WORKR(K) - WORKR(J))*.5                               
C                                                                               
            C(J)     =    C(J)     + (XR*YR + XI*YI)*SCALE              
            CAUX(J) =    CAUX(J) + (XR*YI - XI*YR)*SCALE                
            C(K)     =    C(J)                                          
            CAUX(K) =  - CAUX(J)                                        
C                                                                               
    6     CONTINUE                                                      
C                                                                               
C    Update window pointer                                                      
C                                                                               
          POINT = POINT + WLEN - NVERLP                                 
C                                                                               
    4   CONTINUE                                                        
C                                                                               
C    Inverse fft for correlation computation                                    
C                                                                               
        CALL FFT(C(0), CAUX(0), NFFT, 1)                                
C                                                                               
C  Bye                                                                          
C                                                                               
      RETURN                                                            
      END                                                               
