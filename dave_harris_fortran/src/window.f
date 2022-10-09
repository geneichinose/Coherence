C  WINDOW - Subroutine to window a sequence with various windows.               
C           The sequence is set to zero outside of the window.                  
C                                                                               
C  Author:  Dave Harris                                                         
C                                                                               
C  Created:  August 14, 1981                                                    
C                                                                               
C  Last Modified:  May 13, 1985                                                 
C                                                                               
C  Input Arguments:                                                             
C  ----------------                                                             
C                                                                               
C    X               Real array containing the sequence to be                   
C                    windowed.                                                  
C                                                                               
C    N               The length of the sequence.                                
C                                                                               
C    TYPE            Character*8 variable containing code for                   
C                    the type of window.                                        
C                      'HAM.....'     Hamming window                            
C                      'HAN.....'     Hanning window                            
C                      'R.......'     Rectangular window                        
C                      'C.......'     10 percent cosine taper window            
C                      'T.......'     Triangular window                         
C                                                                               
C    FIRST_SAMPLE    Integer variable containing window first sample            
C                    index.                                                     
C                                                                               
C    WINDOW_LENGTH   Integer variable containing window length in samples.      
C                                                                               
C  Output Arguments:                                                            
C  -----------------                                                            
C                                                                               
C    Y               Real array containing windowed sequence.                   
C                    May be the same array as X for in-place                    
C                    processing.                                                
C                                                                               
C    ERROR_CONDITION Character variable with error condition message,           
C                    if error occurs, blank otherwise.                          
C                                                                               
C  Linkage:  ZERO                                                               
C                                                                               
C  Replacements                                                                 
C                                                                               
C                                                                               
      SUBROUTINE WINDOW(X, N, TYPE, FSAMP, WLEN, Y, ERR)                
C                                                                               
        DIMENSION X(1), Y(1)                                            
        INTEGER FSAMP, LSAMP, WLEN                                      
        CHARACTER*8 TYPE                                                
        CHARACTER*(*) ERR                                               
C                                                                               
C  Error Checking                                                               
C                                                                               
        LSAMP = FSAMP + WLEN - 1                                        
C                                                                               
        IF (  FSAMP .LT. 1 .OR. FSAMP .GT. N ) THEN                     
C                                                                               
          ERR = ' WINDOW - First index out of bounds '                  
          RETURN                                                        
C                                                                               
        ELSE IF ( LSAMP .LT. 1 .OR. LSAMP .GT. N ) THEN                 
C                                                                               
          ERR = ' WINDOW - Window length out of bounds '                
          RETURN                                                        
C                                                                               
        ELSE IF ( WLEN .LE. 0 ) THEN                                    
C                                                                               
          ERR = ' WINDOW - Illegal window length '                      
          RETURN                                                        
C                                                                               
        ELSE                                                            
C                                                                               
          ERR = ' '                                                     
C                                                                               
        END IF                                                          
C                                                                               
        PI = 3.14159265                                                 
C                                                                               
C  Multiply the signal inside the window by the window function.                
C                                                                               
C                                           Rectangular window                  
        IF (  TYPE(1:1) .EQ. 'R' ) THEN                                 
C                                                                               
          DO    1 I = FSAMP, LSAMP                                      
            Y(I) = X(I)                                                 
    1     CONTINUE                                                      
C                                                                               
C                                           Hamming or Hanning window           
        ELSE IF ( TYPE(1:1) .EQ. 'H' ) THEN                             
C                                                                               
          OMEGA = 2.*PI/FLOAT(WLEN - 1)                                 
C                                                                               
C                                                  Hamming window               
          IF (  TYPE(1:3) .EQ. 'HAM' ) THEN                             
            F0 = .54                                                    
            F1 = .46                                                    
C                                                   Hanning window              
          ELSE                                                          
            F0 = .5                                                     
            F1 = .5                                                     
          END IF                                                        
C                                                                               
          DO    2 I = FSAMP, LSAMP                                      
            Y(I) = (F0 + F1*COS(OMEGA*(I - FSAMP) - PI))*X(I)           
    2     CONTINUE                                                      
C                                                                               
C                                           Cosine taper (10%) window           
        ELSE IF ( TYPE(1:1) .EQ. 'C' ) THEN                             
C                                                                               
          ILEN = NINT(WLEN/10.)                                         
          IF (  ILEN .GT. 0 ) THEN                                      
            OMEGA = PI/FLOAT(ILEN)                                      
          END IF                                                        
          IB = FSAMP + ILEN                                             
          DO    3 I = FSAMP, IB-1                                       
            J = I - FSAMP                                               
            Y(I) = X(I)*0.5*(1.-COS(OMEGA*FLOAT(J)))                    
    3     CONTINUE                                                      
          IE = LSAMP - ILEN                                             
          DO    4 I = IB, IE                                            
            Y(I) = X(I)                                                 
    4     CONTINUE                                                      
          DO    5 I = IE+1, LSAMP                                       
            J = LSAMP - I                                               
            Y(I) = X(I)*0.5*(1.-COS(OMEGA*FLOAT(J)))                    
    5     CONTINUE                                                      
C                                                                               
C                                             Triangular window                 
        ELSE IF ( TYPE(1:1) .EQ. 'T' ) THEN                             
C                                                                               
          CENTER = FLOAT(WLEN - 1)/2. + FLOAT(FSAMP)                    
          EXTENT = CENTER - FLOAT(FSAMP)                                
          DO    6 I = FSAMP, LSAMP                                      
            Y(I) = X(I) * (1. - ABS(FLOAT(I) - CENTER)/EXTENT)          
C               I                                                               
    6     CONTINUE                                                      
C                                                                               
        ELSE                                                            
C                                                                               
          ERR = ' WINDOW -  Undefined window type '                     
          RETURN                                                        
C                                                                               
        END IF                                                          
C                                                                               
C  Zero that part of the signal outside of the window                           
C                                                                               
        CALL ZERO(Y(1), FSAMP - 1)                                      
        CALL ZERO(Y(LSAMP + 1), N - LSAMP)                              
C                                                                               
C  Bye                                                                          
C                                                                               
      RETURN                                                            
      END                                                               
