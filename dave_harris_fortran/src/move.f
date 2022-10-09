C  MOVE - Subroutine to copy one real array into another.                       
C                                                                               
C  Author:  Dave Harris                                                         
C                                                                               
C  Created:  November 11, 1981                                                  
C                                                                               
C  Last Modified:  April 13, 1984                                               
C                                                                               
C  Input arguments:                                                             
C  ----- ----------                                                             
C                                                                               
C    B                           Array to be copied.                            
C                                                                               
C    N                           Number of elements in B.                       
C                                                                               
C  Output Arguments:                                                            
C  ------ ----------                                                            
C                                                                               
C    A                           Array containing copy.                         
C                                                                               
C  Linkage:  (none)                                                             
C                                                                               
C                                                                               
      SUBROUTINE MOVE(A, B, N)                                          
C                                                                               
        REAL*4 A(1), B(1)                                               
C                                                                               
        DO    1 I = 1, N                                                
          A(I) = B(I)                                                   
C             I                                                                 
    1   CONTINUE                                                        
C                                                                               
      RETURN                                                            
      END                                                               
