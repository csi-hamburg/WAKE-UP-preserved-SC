#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
BASEDIR="$DIR/../../.."

INDIR="$BASEDIR/derivatives/surface_maps/86"
OUTDIR="$BASEDIR/derivatives/figures/surface_plots/86"

IFS=$'\n' ## allow spaces in file names

for f in $(ls "$INDIR"/**/*.tiff); do
	file=$(basename $f)
	filename="${file%.*}"
	
	echo $f	
	echo $file
	echo $filename

	#exit

	flag="${filename##*_}"
	if [[ $flag == "cropped" ]]; then
		continue
	fi
	
	prefix="${filename%%_*}"
	if [[ $prefix == "colorbar" ]]; then
		bgcolor=white
	else
		bgcolor=black
	fi

	convert -trim "$f" -transparent $bgcolor -resize x310 +repage "$(dirname "$f")/${filename}_cropped.tiff" 
done

#exit
echo test

montage -mode concatenate -tile 2x2 -border 5 -bordercolor transparent -title 'Placebo'  -pointsize 30 \
	"$INDIR"/e.abs.Placebo.visit/e.abs.Placebo.visit0_cropped.tiff "$INDIR"/e.abs.Placebo.visit/e.abs.Placebo.visit180_cropped.tiff \
 	"$INDIR"/p.Placebo.visit/p.Placebo.visit0_cropped.tiff "$INDIR"/p.Placebo.visit/p.Placebo.visit180_cropped.tiff \
	visit_Placebo_2x2.png


montage -mode concatenate -tile 2x2 -border 5 -bordercolor transparent -title 'Alteplase'  -pointsize 30 \
	"$INDIR"/e.abs.rtPA.visit/e.abs.rtPA.visit0_cropped.tiff "$INDIR"/e.abs.rtPA.visit/e.abs.rtPA.visit180_cropped.tiff \
 	"$INDIR"/p.rtPA.visit/p.rtPA.visit0_cropped.tiff "$INDIR"/p.rtPA.visit/p.rtPA.visit180_cropped.tiff \
	visit_rtPA_2x2.png


montage -mode concatenate -tile 2x1 -border 50 -bordercolor transparent \
	visit_Placebo_2x2.png visit_rtPA_2x2.png \
	visit_4x2.png

convert visit_4x2.png \
	-gravity north -pointsize 75 -annotate +40+30 'Time' \
	-gravity northwest -annotate +30+30 'a' \
	-pointsize 60 -rotate -270 -gravity north -annotate +75+40 'IP(T2 > T1)' -rotate 270 \
	-pointsize 60 -rotate -270 -gravity north -annotate -275+40 '-log(P)' -rotate 270 \
	 visit_4x2.png


montage -mode concatenate -tile 1x2 -border 5 -bordercolor transparent \
	"$INDIR"/e.abs.visit/colorbar_e.abs.visit_cropped.tiff "$INDIR"/p.visit/colorbar_p.visit_cropped.tiff \
	visit_colorbar_1x2.png



montage -mode concatenate -tile 2x1 -border 5 -bordercolor transparent -gravity south\
	visit_4x2.png visit_colorbar_1x2.png \
	visit_5x2.png




montage -mode concatenate -tile 2x2 -border 5 -bordercolor transparent -title 'Before randomisation'  -pointsize 30 \
	"$INDIR"/e.abs.V0.treatment/e.abs.V0.treatment0_cropped.tiff "$INDIR"/e.abs.V0.treatment/e.abs.V0.treatment180_cropped.tiff \
 	"$INDIR"/p.V0.treatment/p.V0.treatment0_cropped.tiff "$INDIR"/p.V0.treatment/p.V0.treatment180_cropped.tiff \
	treatment_V0_2x2.png



montage -mode concatenate -tile 2x2 -border 5 -bordercolor transparent -title '22-36 hours after'  -pointsize 30 \
	"$INDIR"/e.abs.V3.treatment/e.abs.V3.treatment0_cropped.tiff "$INDIR"/e.abs.V3.treatment/e.abs.V3.treatment180_cropped.tiff \
 	"$INDIR"/p.V3.treatment/p.V3.treatment0_cropped.tiff "$INDIR"/p.V3.treatment/p.V3.treatment180_cropped.tiff \
	treatment_V3_2x2.png



montage -mode concatenate -tile 2x1 -border 50 -bordercolor transparent\
	treatment_V0_2x2.png treatment_V3_2x2.png \
	treatment_4x2.png

convert treatment_4x2.png \
	-gravity north -pointsize 75 -annotate +40+30 'Treatment' \
	-gravity northwest -annotate +30+30 'b' \
	-pointsize 60 -rotate -270 -gravity north -annotate +75+40 'IP(P > A)' -rotate 270 \
	-pointsize 60 -rotate -270 -gravity north -annotate -275+40 '-log(P)' -rotate 270 \
	treatment_4x2.png

montage -mode concatenate -tile 1x2 -border 5 -bordercolor transparent \
	"$INDIR"/e.abs.treatment/colorbar_e.abs.treatment_cropped.tiff "$INDIR"/p.treatment/colorbar_p.treatment_cropped.tiff \
	treatment_colorbar_1x2.png



montage -mode concatenate -tile 2x1 -border 5 -bordercolor transparent -gravity south\
	treatment_4x2.png treatment_colorbar_1x2.png \
	treatment_5x2.png


montage -mode concatenate -tile 1x2 -border 5 -bordercolor transparent \
	visit_5x2.png treatment_5x2.png \
	vt_5x4.png


montage -mode concatenate -tile 3x2 -border 20 -bordercolor transparent -title "Interaction Time:Treatment" -pointsize 19  \
	"$INDIR"/e.ix/e.ix0_cropped.tiff "$INDIR"/e.ix/e.ix180_cropped.tiff "$INDIR"/e.ix/colorbar_e.ix_cropped.tiff \
 	"$INDIR"/p.ix/p.ix0_cropped.tiff "$INDIR"/p.ix/p.ix180_cropped.tiff "$INDIR"/p.ix/colorbar_p.ix_cropped.tiff \
	ix_comp.png


convert ix_comp.png -resize 200% \
	-gravity northwest -pointsize 75 -annotate +30+-15 'c' \
	 -pointsize 60 -rotate -270 -gravity north -annotate +250-0 'IP(ΔP > ΔA)' -rotate 270 \
	 -pointsize 60 -rotate -270 -gravity north -annotate -500-0 '-log(P)' -rotate 270 \
	ix_comp_double.png

montage -mode concatenate -tile 2x1 -border 15 -bordercolor transparent -gravity center \
	vt_5x4.png ix_comp_double.png \
	surface.png

mv surface.png "$OUTDIR"
mv *.png "$OUTDIR/aux"
