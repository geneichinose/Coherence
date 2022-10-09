#!/bin/csh

/bin/rm -f *.sac

sac << EOF
cut t2 n 3500
read *.cor
rmean
taper type cosine width 0.05
write append .cut
quit
EOF

cat >! cohere.in << EOF
2016.006.01.15.00.0195.IC.MDJ.00.BHZ.M.SAC.cor.cut
2009.145.00.45.00.0248.IC.MDJ.00.BHZ.M.SAC.cor.cut
20
100
5
EOF

cohere < cohere.in

# Enter names of input files
# Filename:
#2009.145.00.45.00.0248.IC.MDJ.00.BHZ.M.SAC.cor.cut
# Filename:
#2016.006.01.15.00.0195.IC.MDJ.00.BHZ.M.SAC.cor.cut
# Data window length(sec):
#5 
# Number of windows:
#100
# Correlation window length(sec):
#5
# 2009.145.00.45.00.0248.IC.MDJ.00.BHZ.M.SAC.cor.cut                                                                                      3500
# 2016.006.01.15.00.0195.IC.MDJ.00.BHZ.M.SAC.cor.cut                              
