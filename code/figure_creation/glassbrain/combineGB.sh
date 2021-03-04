#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
BASEDIR="$DIR/../../.."

OUTDIR="$BASEDIR/derivatives/figures/glassbrain"
if [ ! -d "$OUTDIR" ]; then
	mkdir -p "$OUTDIR"
fi


for visit in V0 V3; do
	echo $visit
	for asz in 86 116; do

		INDIR="$BASEDIR/derivatives/NeMo_output/$visit/$asz"

		convert  "$INDIR/GBmeanChaCo_coronalfigure_${visit}_${asz}.tif" \
			-gravity Center -crop 730x730+20+-20 +repage \
			-undercolor '#ffc0cb' -gravity east  -pointsize 36 -annotate +30+0 'L' \
			-undercolor '#ffc0cb' -gravity west  -pointsize 36 -annotate +60+0 'R' \
			cor.png

		convert  "$INDIR/GBmeanChaCo_axialfigure_${visit}_${asz}.tif" \
			-gravity Center -crop 730x730+20+-20 +repage \
			-undercolor '#ffc0cb' -gravity east  -pointsize 36 -annotate +100+0 'A' \
			-undercolor '#ffc0cb' -gravity west  -pointsize 36 -annotate +40+0 'P' \
			ax.png


		montage -mode concatenate -tile 2x1 -border 0  -gravity Center \
			-pointsize 25 -label "coronal"  cor.png \
			-label "axial" ax.png \
			"$OUTDIR/GBall_${visit}_${asz}.png"

		rm ax.png cor.png
	
	done
done
