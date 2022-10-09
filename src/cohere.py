#!/usr/bin/env python3

######
###### computes coherence between pairs of files 
###### list of files are read from command line 
###### using wildcards. Outputs plots and csv of (freq,Cxy)
###### and also a heatmap of all the Cxy(f,sta-pairs)
######
import sys
import numpy as np
import obspy
# from obspy              import read
# from obspy.core         import UTCDateTime
import matplotlib.pyplot as plt
import scipy.signal as sp
import seaborn as sns
import pandas as pd

######
###### start program, get command-line args
######
nargs = int(len(sys.argv)) - 1
args  = str(sys.argv)

if( nargs == 0 ):
	print( "error need more than 0 args got %d " % nargs )
	sys.exit()

#### debug
# print("nargs = " + str(nargs))
# for i in range( 0, nargs+1, 1 ):
#	print( " i = " + str(i) + " argv = " + sys.argv[i] )

#
# the first argv is the name of the array
#
array_name = str( sys.argv[1] )

# reads into single stream
#
st = obspy.read( sys.argv[2] )
print("adding file " + sys.argv[2] )
for i in range( 3, nargs + 1, 1 ):
	print("adding file " + sys.argv[i] )
	st += obspy.read( sys.argv[i] )
print( " Read number of files into stream=" + str(len(st)) + " nargs = " + str(nargs+1) )

#
# loop over each trace in stream and extract trace(tr1) and next trace(tr2)
# for processing
# 
npairs = len(st)-1
coh = np.array([])
index_list = []

for i in range( 0, len(st)-1, 1 ):
	tr1 = st[i]
	tr2 = st[i+1]
	tr1.detrend()
	tr2.detrend()
	tr1.taper( 0.1, type='cosine', side='both' )
	tr2.taper( 0.1, type='cosine', side='both' )

	start_date_string = tr1.stats.starttime.strftime("%Y-%m-%dT%H%M")
	index_string = tr1.stats.station + "-" + tr2.stats.station

	# fs = sampling frequency = 1 / dt
	sampling_frequency = 1 / tr1.stats.delta
	print( sampling_frequency )
	print( tr1.stats.delta )
	print( "Calling scipy.coherence: " + index_string + " i=" + str(i) + " fs=" + str(sampling_frequency) + " npts = " + str(tr1.stats.npts) )
	f, Cxy = sp.coherence( tr1.data, tr2.data, fs=sampling_frequency )
	print(f.shape, f.size)
	# print(Cxy.shape, Cxy.size)
	index_list.append(index_string)
	coh = np.append( coh, Cxy, axis=0 )

	####
	#### single plots, each sta-pair plot(freq,Cxy)
	####
	plt.figure(i)
	fig, ax = plt.subplots(figsize=(8,8))
	ax.plot( f, Cxy, color='blue', linewidth=2 )
	plt.title( "DAS channel pairs: " + index_string, fontsize=16 )
	plt.ylabel('Coherency', fontsize=14)
	plt.xlabel('Frequency (Hz)', fontsize=14)
	plt.grid( axis='both', which="both", color='black', linestyle='dashed', linewidth=0.5 )

	fmin = 0.0
	fmax = 0.5*sampling_frequency
	xaxis_major_ticks = np.arange( fmin, fmax, np.around( fmax/5.0, decimals=1 ))
	xaxis_minor_ticks = np.arange( fmin, fmax, np.around( fmax/25.0, decimals=1 ))

	ax.set_xticks( xaxis_major_ticks, minor=False )
	ax.set_xticks( xaxis_minor_ticks, minor=True )
	ax.set_yticks( np.arange( 0, 1, 0.2), minor=False )
	ax.set_yticks( np.arange( 0, 1, 0.1), minor=True )

	ax.tick_params(axis='both', which='minor', labelsize=12)
	ax.tick_params(axis='both', which='major', labelsize=14)

	filename = 'coherence.' + str(i).zfill(3) + '.png' 
	print(filename)
	plt.savefig(filename, dpi=300, format='png')

	####
	#### write out coh spec to CSV file
	####
	# print(f)
	# print(Cxy) 
	# print( type(f) )
	# print( type(Cxy) )
	df1 = pd.DataFrame( {"frequency" : f, "Coherence" : Cxy } )

	filename = 'coherence.' + str(i).zfill(3) + '.csv' 
	df1.to_csv( filename, index=False )

####
#### create Pandas dataframe of the coherence spectra
#### dataframe = Cxy( row, col ) row=sensor-pair, col=frequency, Cxy
####
# print( coh.shape, coh.size )
coh = np.reshape( coh, [npairs, 129], order='C' )
print( coh.shape, coh.size )
freq = np.around( f, decimals=1 )
df = pd.DataFrame(coh, columns=freq, index=index_list)

####
#### create a seaborn.heatmap plot of all the dataframe
####
plt.figure(i+1)
fig, ax = plt.subplots(figsize=(12,4))

sns.heatmap(df, vmin=-0.0001, vmax=1.0001, xticklabels=10, cbar_kws={'label': 'Coherency'}, ax=ax )

ax.set_xlabel('Frequency (Hz)' )
ax.set_ylabel('DAS Channel Pairs')
ax.set_title( array_name + ' Coherency for DAS channel pairs (' + start_date_string + ')' )

# old_ticks = ax.get_xticks()
# print(old_ticks)

fmin = 0.0
fmax = 0.5*sampling_frequency
xaxis_major_ticks = np.arange( fmin, len(df.columns), len(df.columns)/10 )
xaxis_minor_ticks = np.arange( fmin, len(df.columns), len(df.columns)/50 )
xaxis_major_ticks_norm = np.around( fmax * ( xaxis_major_ticks / len(df.columns) ), decimals=1 )

# print( xaxis_major_ticks )
# print( xaxis_minor_ticks )

ax.set_xticks( xaxis_major_ticks, minor=False )
ax.set_xticks( xaxis_minor_ticks, minor=True )
ax.set_xticklabels( xaxis_major_ticks_norm.astype(str) )

# ax.set_xticks( np.arange( 0, len(df.columns), len(df.columns)/10 ), minor=False )
# ax.set_xticks( np.arange( 0, len(df.columns), len(df.columns)/50 ), minor=True )
# # ax.set_xticks( np.arange( 0, 129, 12.9 ) )
# ax.set_xticklabels( ["0", "5", "10", "15", "20", "25", "30", "35", "40", "45" ] )

# new_ticks = ax.get_xticks()
# print(new_ticks)

ax.grid( axis='both', which="both", color='gray', linestyle='dashed', linewidth=0.5 )

colorbar = ax.collections[0].colorbar
colorbar.set_ticks([0.0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0] )

filename = "heatmap." + array_name + "." + start_date_string + ".png"
plt.savefig( filename, dpi=600, format='png' )
# plt.show()
