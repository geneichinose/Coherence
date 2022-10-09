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
80
 Number of windows:
10
 Correlation window length(sec):
50
EOF


