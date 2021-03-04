#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
BASEDIR="$DIR/../../.."

OUTDIR="$BASEDIR/derivatives/figures/lesion_overlay"

for lesions in original derivatives; do
for bramford in all PACS; do
	for visit in V0 V3; do


		OUTFILE="$OUTDIR/heatmap_${lesions}_${visit}_${bramford}.nii.gz"

		if [ $visit == "V0" ]; then
			postfix="-v00_MNI_lesion_mask_bin_dil"
		elif [ $visit == "V3" ]; then
			postfix="-v03LesionMaskToMNI"
		fi


		if [ ! -f "$OUTFILE" ]; then
			echo "creating lesion overlay of $bramford strokes at visit $visit"

			while read ID; do
				f="$BASEDIR/lesionmasks/$lesions/$visit/${ID}${postfix}.nii.gz"
				if [ ! -f "$OUTFILE" ]; then
					fslmaths "$f" -mul 0 "$OUTFILE"
				fi
				echo $f
				

				fslmaths "$OUTFILE" -add "$f" "$OUTFILE"

			done < "$BASEDIR/derivatives/subjectsV0V3${bramford}.dat"

		else
			echo "$bramford stroke lesion overlay at visit $visit already exists."
		fi


## render with fsl
		. createlightboxview.sh
		for minno in 5 10; do
##axial
			zaxis=2
			slicespacing=5
			zrangemin=50
			zrangemax=140
			render_fsl "$OUTFILE" $zaxis $slicespacing $zrangemin $zrangemax "${OUTFILE}_axial_min${minno}.png" $minno

##coronal
			zaxis=1
			slicespacing=6.5
			zrangemin=63
			zrangemax=180
			render_fsl "$OUTFILE" $zaxis $slicespacing $zrangemin $zrangemax "${OUTFILE}_coronal_min${minno}.png" $minno
		done
	done

done
done
