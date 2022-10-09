C  COHERE -- Code to calculate psuedo-coherence of transients.                  
C                                                                               
C  Author:  Dave Harris                                                         
C                                                                               
C  Created:  June 8, 1982                                                       
C                                                                               
C  Last Modified:  June 20, 1984                                                
C                                                                               
C                                                                               
C                                                                               
C  Linkage:  CRSCOR, FFT, ZERO, RSAC1, WSAC1                                    
C                                                                               
C                                                                               
      PROGRAM COHERE                                                    
C                                                                               
      DIMENSION DATA1(60000), DATA2(60000)                              
      DIMENSION CROSS(16384), AUTO1(16384)                              
      DIMENSION WORK(16384), AUTO2(16384)                               
      DIMENSION SELFN1(16384), SELFN2(16384)                            
      CHARACTER*128 FILE1, FILE2, OUTFIL                                 
      CHARACTER*130 ERRMSG                                              
      INTEGER START, SIZE                                               
      REAL*4 SUM, MEAN                                                  
C                                                                               
C                                                                               
C  Prompt for required parameters                                               
C                                                                               
      WRITE(6,*) 'Enter names of input files'                           
      WRITE(6,*) 'Filename:'                                            
      READ(5,'(A128)') FILE1                                             
      WRITE(6,*) 'Filename:'                                            
      READ(5,'(A128)') FILE2                                             
C     WRITE(TERMOUT,*) 'Output file name:'                                      
C     READ(TERMIN,'(A32)') OUTFILE                                              
      WRITE(6,*) 'Data window length(sec):'                             
      READ(5,*) TWLEN                                                   
      WRITE(6,*) 'Number of windows:'                                   
      READ(5,*) NWIN                                                    
      WRITE(6,*) 'Correlation window length(sec):'                      
      READ(5,*) CLEN                                                    
C                                                                               
C  Checking                                                                     
C                                                                               
      IF (  CLEN .GT. TWLEN ) THEN                                      
        WRITE(6,*) '*** Correlation length may not exceed window length 
     1***'                                                              
        CLEN = TWLEN                                                    
      END IF                                                            
C                                                                               
C  Read data and remove means                                                   
C                                                                               
      CALL ZERO(DATA1, 60000)                                           
      CALL RSAC1(FILE1, DATA1, NDATA, BEGIN, DELTA, 60000, IERR)        
      SUM = 0.                                                          
      DO    1 I = 1, NDATA                                              
        SUM = SUM + DATA1(I)                                            
C           I                                                                   
    1 CONTINUE                                                          
      MEAN = SUM/NDATA                                                  
      DO    2 I = 1, NDATA                                              
        DATA1(I) = DATA1(I) - MEAN                                      
C           I                                                                   
    2 CONTINUE          

	write(*,*)FILE1,NDATA                                                
C                                                                               
      CALL ZERO(DATA2, 60000)                                           
      CALL RSAC1(FILE2, DATA2, NDATA, BEGIN, DELTA, 60000, IERR)        
      SUM = 0.                                                          
      DO    3 I = 1, NDATA                                              
        SUM = SUM + DATA2(I)                                            
C           I                                                                   
    3 CONTINUE                                                          
      MEAN = SUM/NDATA                                                  
      DO    4 I = 1, NDATA                                              
        DATA2(I) = DATA2(I) - MEAN                                      
C           I                                                                   
    4 CONTINUE                                                          

	write(*,*)FILE2,NDATA
C                                                                               
C  Check window size to see that it is in bounds                                
C                                                                               
      NWINDS = INT(TWLEN/DELTA + 1.)                                    
      NLAGS = INT(CLEN/DELTA + 1.)                                      
      IF (  NWINDS .GT. 4096 ) THEN                                     
        WLMAX = (4096 - 1) * DELTA                                      
        WRITE(6,*) ' *** COHERE -- Maximum window length:  ',WLMAX      
        CALL EXIT                                                       
      END IF                                                            
C                                                                               
C  Calculate cross-correlation                                                  
C                                                                               
      CALL CRSCOR(DATA2, DATA1, NDATA, NWIN, NWINDS,                    
     1            'HAMMING', CROSS, NFFT, ERRMSG)                       
C                                                                               
C  Compute cross-spectrum                                                       
C                                                                               
      NSHIFT = NFFT/2                                                   
      IFLDPT = NSHIFT + 1                                               
      IMGIND = NFFT + 1                                                 
      START = IFLDPT - NLAGS                                            
      SIZE  = 2 * NLAGS + 1                                             
      CALL ZERO(CROSS(IMGIND), NFFT)                                    
      CALL SHIFT(CROSS, NFFT, NSHIFT, 'C       ', WORK, ERRMSG)         
      CALL WINDOW(WORK, NFFT, 'TRIANGLE', START, SIZE, WORK, ERRMSG)    
      CALL SHIFT(WORK, NFFT, -NSHIFT, 'C       ', CROSS, ERRMSG)        
      CALL FFT(CROSS, CROSS(IMGIND), NFFT, -1)                          
      DO    5 I = 1, IFLDPT                                             
        CROSS(I) = SQRT(CROSS(I)**2 + CROSS(IMGIND + I - 1)**2)         
C           I                                                                   
    5 CONTINUE                                                          
C                                                                               
C  Compute autospectrum of file1 signal                                         
C                                                                               
      CALL CRSCOR(DATA1, DATA1, NDATA, NWIN, NWINDS,                    
     1            'HAMMING', AUTO1, NFFT, ERRMSG)                       
      CALL ZERO(AUTO1(IMGIND), NFFT)                                    
      CALL SHIFT(AUTO1, NFFT, NSHIFT, 'C       ', WORK, ERRMSG)         
      CALL WINDOW(WORK, NFFT, 'TRIANGLE', START, SIZE, WORK, ERRMSG)    
      CALL SHIFT(WORK, NFFT, -NSHIFT, 'C       ', AUTO1, ERRMSG)        
      CALL FFT(AUTO1, AUTO1(IMGIND), NFFT, -1)                          
C                                                                               
C  Compute autospectrum of file2 signal                                         
C                                                                               
      CALL CRSCOR(DATA2, DATA2, NDATA, NWIN, NWINDS,                    
     1            'HAMMING', AUTO2, NFFT, ERRMSG)                       
      CALL ZERO(AUTO2(IMGIND), NFFT)                                    
      CALL SHIFT(AUTO2, NFFT, NSHIFT, 'C       ', WORK, ERRMSG)         
      CALL WINDOW(WORK, NFFT, 'TRIANGLE', START, SIZE, WORK, ERRMSG)    
      CALL SHIFT(WORK, NFFT, -NSHIFT, 'C       ', AUTO2, ERRMSG)        
      CALL FFT(AUTO2, AUTO2(IMGIND), NFFT, -1)                          
C                                                                               
C  Compute Coherence and Self Noise traces                                      
C                                                                               
      FACTOR = 2. * DELTA / ( NWIN * NWINDS )                           
C------------------------------------------------------------                   
C  WHY THE 2 in FACTOR is need to match SAC/SPE is not CLEAR                    
C------------------------------------------------------------                   
      DO    6 I = 1, IFLDPT                                             
	AUTO1(I) = AUTO1(I) * FACTOR                                           
	AUTO2(I) = AUTO2(I) * FACTOR                                           
	CROSS(I) = CROSS(I) * FACTOR                                           
        WORK(I) = CROSS(I)/SQRT(ABS(AUTO1(I)))/SQRT(ABS(AUTO2(I)))      
C	SELFN1(I) = ABS(AUTO1(I)) - CROSS(I)                                          
C	SELFN2(I) = ABS(AUTO2(I)) - CROSS(I)                                          
C           I                                                                   
    6 CONTINUE                                                          
C                                                                               
C  Write coherence file                                                         
C                                                                               
      DELTAF = 1./(DELTA * FLOAT(NFFT))                                 
      CALL WSAC1('coh.ab', WORK, IFLDPT, 0., DELTAF, IERR)              
C     CALL WSAC1('paa', AUTO1, FOLDING_POINT, 0., DELTAF, ERROR_FLAG)           
C     CALL WSAC1('pbb', AUTO2, FOLDING_POINT, 0., DELTAF, ERROR_FLAG)           
C     CALL WSAC1('pnn', SELFN1, FOLDING_POINT, 0., DELTAF, ERROR_FLAG)          
C     CALL WSAC1('pmm', SELFN2, FOLDING_POINT, 0., DELTAF, ERROR_FLAG)          
C                                                                               
C  Bye                                                                          
C                                                                               
      CALL EXIT                                                         
      END                                                               
