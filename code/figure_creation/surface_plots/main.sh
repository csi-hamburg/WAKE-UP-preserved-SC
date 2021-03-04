#!/bin/bash


export FREESURFER_HOME=/usr/local/freesurfer
export FS_LICENSE=$FREESURFER_HOME/license.txt 
source $FREESURFER_HOME/SetUpFreeSurfer.sh

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
BASEDIR="$DIR/../../.."






IFS=$'\n' ## allow spaces in file names

for atlas in  86 116; do
INDIR="$BASEDIR/derivatives/surface_maps/$atlas"

if [ $atlas == "86" ]; then
	SUBJECT=fsaverage	
	export SUBJECTS_DIR=/usr/local/freesurfer/subjects/
	annot=aparc.annot
elif [ $atlas == "116" ]; then
	SUBJECT=spm_single_sub_resamp
	export SUBJECTS_DIR=/home/eckhard/Documents/Research/Projects/Medicine/WAKE-UP\ NeMo\ gitmake/code/figure_creation/surface_plots/aal_single_labels/aal_atlas
	annot=aal
fi
source $FREESURFER_HOME/SetUpFreeSurfer.sh
mri_annotation2label --subject $SUBJECT --hemi lh --annotation $annot --border borderfile${atlas}.mgh --outdir ./ --surface inflated

 --
for f in $(ls -d "$INDIR"/*/); do
	g=$(basename $f)
	#g="$INDIR/$f/$f"
	g="$f/$g"
	echo $g
	
	cp "${g}.ctab" aparc.temp.ctab
	tksurfer $SUBJECT lh inflated\
		 -labels-under\
		 -overlay borderfile${atlas}.mgh\
		 -annot $annot\
		 -colortable aparc.temp.ctab\
		 -tcl rotate.tcl
	#rm -rf "/${g}.rgb"
	#mkdir "${g}.rgb"
	#mv "$INDIR"/*-capture.rgb "$f.rgb"
	rm aparc.temp.ctab
	
	mv temp0.tiff "${g}0.tiff"
	mv temp180.tiff "${g}180.tiff"
	
	continue

## create video of rotating brain
ffmpeg -framerate 25 -f image2 -i $f.rgb/%d-capture.rgb -c:v libx264 -profile:v high -crf 20 -pix_fmt yuv420p $f.rgb/$f.mp4
done
done
exit
