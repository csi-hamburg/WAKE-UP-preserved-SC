#/bin/bash

## extract and save lesion volumes form WAKE UP lesion masks

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
BASEDIR="$DIR/../.."
INDIR="$BASEDIR"/lesionmasks/derivatives
OUTDIR="$BASEDIR"/derivatives/


for visit in V0 V3
	do
	OUTFILE="$OUTDIR"/volumes$visit.dat
	if [ -f "$OUTFILE" ]; then
		rm "$OUTFILE"
	fi
	for f in $(ls "$INDIR"/$visit); do
		vol=$(fslstats "$INDIR/$visit/$f" -V | awk '{print $1}')
	        echo "$f $vol" >> "$OUTFILE"	
	done
done
