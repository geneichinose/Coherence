C                                                    RMS                        
C                                                                               
C  REAL*4 function to compute the rms value of an array of samples              
C                                                                               
C  Author:  Dave Harris                                                         
C                                                                               
C  Created:  April 16, 1984                                                     
C                                                                               
C  Last Modified:  April 16, 1984                                               
C                                                                               
C  Replacements                                                                 
C                                                                               
C                                                                               
C                                                                               
C  Input arguments:                                                             
C  ----- ----------                                                             
C                                                                               
C    X                             REAL*4 Array of samples                      
C                                                                               
C    #SAMPLES                      Number of samples                            
C                                                                               
C                                                                               
C  Output Arguments:                                                            
C  ------ ----------                                                            
C                                                                               
C    RMS                           RMS value of samples                         
C                                                                               
C  Linkage:  none                                                               
C                                                                               
C                                                                               
      REAL*4 FUNCTION RMS(X, NSAMPS)                                    
C                                                                               
        REAL*4 X(1)                                                     
C                                                                               
        RMS = 0.                                                        
        DO    1 I = 1, NSAMPS                                           
          RMS = RMS + X(I)**2                                           
C             I                                                                 
    1   CONTINUE                                                        
        RMS = SQRT(RMS)                                                 
C                                                                               
      RETURN                                                            
      END                                                               
