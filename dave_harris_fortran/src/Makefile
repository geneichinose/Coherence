#
# Fortran compiler flags
#
FC = gfortran
#FFLAGS = -g -Nq300
FFLAGS = -g 
INSTALL = $(HOME)/bin

OBJECTS =  cohere.o crscor.o fft.o zero.o shift.o \
	   rms.o window.o move.o 

#LIBS = /Users/ichinose1/Work/sac32/lib/libsacio.a
LIBS = /Users/ichinose1/Work/sac64/lib/libsacio.a

cohere: $(OBJECTS)
	$(FC) $(FFLAGS) $(OBJECTS) -o cohere $(LIBS) 
	mv cohere $(INSTALL)

clean :
	/bin/rm -f *.o cohere
