C  SHIFT - Subroutine to shift a sequence.                                      
C          Circular and linear (with zero filling) shifts                       
C          are implemented.                                                     
C                                                                               
C  Author:  Dave Harris                                                         
C                                                                               
C  Last Modified:  January 3, 1985                                              
C                                                                               
C                                                                               
C  Arguments:                                                                   
C  ----------                                                                   
C                                                                               
C    X            Array containing the sequence to be shifted.                  
C                                                                               
C    N            Length of the sequence.                                       
C                                                                               
C    ISHFT        Number of samples to shift.                                   
C                                                                               
C                   A negative number indicates a shift left.                   
C                   A positive number indicates a shift right.                  
C                   A zero indicates no shift.                                  
C                                                                               
C    TYPE         Character*8 variable indicating which type                    
C                   of shift.                                                   
C                                                                               
C                   'C.......'    Circular shift                                
C                   'L.......'    Linear shift with zero filling.               
C                                                                               
C  Output Arguments                                                             
C  -----------------                                                            
C                                                                               
C    Y            Array containing shifted sequence.                            
C                   May not be the same array as X.                             
C                                                                               
C  ERROR_MESSAGE  Character*130 variable containing error message               
C                 when error detected, ' ' when no error.                       
C                                                                               
C  Linkage: (none)                                                              
C                                                                               
C                                                                               
      SUBROUTINE SHIFT(X, N, ISHFT, TYPE, Y, ERRMSG)                    
C                                                                               
        DIMENSION X(1), Y(1)                                            
        CHARACTER*8 TYPE                                                
        CHARACTER*130 ERRMSG                                            
C                                                                               
C  Initializations                                                              
C                                                                               
        ERRMSG = ' '                                                    
C                                                                               
C  Error checking                                                               
C                                                                               
C                                         Shift too large                       
        IF (  IABS(ISHFT) .GE. N ) THEN                                 
C                                                                               
          ERRMSG = ' SHIFT *** shift larger than data record *** '      
C                                                                               
        ELSE IF ( TYPE(1:1) .NE. 'C' .AND. TYPE(1:1) .NE. 'L' ) THEN    
C                                                                               
          ERRMSG = ' SHIFT *** illegal shift type *** '                 
C                                                                               
C                                         Everything OK                         
        ELSE                                                            
C                                                                               
C                                          Shift right                          
          IF (  ISHFT .GE. 0 ) THEN                                     
C                                                                               
            M = N - ISHFT                                               
            DO    1 I = 1, M                                            
              Y(I + ISHFT) = X(I)                                       
    1       CONTINUE                                                    
C                                      Circular shift                           
            IF (  TYPE(1:1) .EQ. 'C' ) THEN                             
              DO    2 I = 1, ISHFT                                      
                Y(I) = X(N - ISHFT + I)                                 
    2         CONTINUE                                                  
C                                       Linear shift (zero filling)             
            ELSE                                                        
              DO    3 I = 1, ISHFT                                      
                Y(I) = 0.                                               
    3         CONTINUE                                                  
            END IF                                                      
C                                                                               
C                                         Shift left                            
          ELSE IF ( ISHFT .LT. 0 ) THEN                                 
C                                                                               
            IS = -ISHFT                                                 
            M = N - IS                                                  
            DO    4 I = 1, M                                            
              Y(I) = X(I + IS)                                          
    4       CONTINUE                                                    
C                                       Circular shift                          
            IF (  TYPE(1:1) .EQ. 'C' ) THEN                             
              DO    5 I = 1, IS                                         
                Y(N - IS + I) = X(I)                                    
    5         CONTINUE                                                  
C                                        Linear shift (zero filling)            
            ELSE                                                        
              DO    6 I = 1, IS                                         
                Y(N - IS + I) = 0.                                      
    6         CONTINUE                                                  
            END IF                                                      
C                                                                               
          END IF                                                        
C                                                                               
        END IF                                                          
C                                                                               
C  Bye                                                                          
C                                                                               
      RETURN                                                            
      END                                                               
