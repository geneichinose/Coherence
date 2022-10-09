C                                                                               
C  AUTHOR:  DAVE HARRIS                                                         
C           L-205                                                               
C           LAWRENCE LIVERMORE NATIONAL LABORATORY                              
C           LIVERMORE, CA  94550                                                
C                                                                               
C  Last Modified:  February 16, 1982                                            
C                                                                               
C  INPUT ARGUMENTS:                                                             
C  ----------------                                                             
C                                                                               
C    X             VECTOR OR ARRAY TO BE ZEROED                                 
C                                                                               
C    N             LENGTH OF VECTOR OR ARRAY.                                   
C                  IF ZEROING AN ARRAY OF DIMENSION                             
C                  P X Q, USE N=PQ.                                             
C                                                                               
      SUBROUTINE ZERO(X,N)                                              
C                                                                               
        DIMENSION X(1)                                                  
C                                                                               
        IF (  N .GT. 0 ) THEN                                           
          DO    1 I=1, N                                                
            X(I)=0.                                                     
    1     CONTINUE                                                      
        END IF                                                          
C                                                                               
        RETURN                                                          
      END                                                               
