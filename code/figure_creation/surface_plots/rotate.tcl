set gaLinkedVars(fopaqueflag) 0
SendLinkedVarGroup overlay

set gaLinkedVars(fthresh) .5
set gaLinkedVars(fmid) 1.0
set gaLinkedVars(fslope) 0.0
SendLinkedVarGroup overlay

set gaLinkedVars(labelstyle) 0
SendLinkedVarGroup label

set gaLinkedVars(curvflag) 0
SendLinkedVarGroup view


set gaLinkedVars(light0) 0.4
set gaLinkedVars(light1) 0.6
set gaLinkedVars(light2) 0.1
set gaLinkedVars(light3) 0.7
set gaLinkedVars(offset) 0.75
SendLinkedVarGroup scene


labl_load_color_table aparc.temp.ctab
redraw

after 10000

save_tiff temp0.tiff
rotate_brain_y 180
redraw
save_tiff temp180.tiff

exit

for {set i 0} {$i < 36} {incr i} {
	rotate_brain_y 10
	redraw
	after 5
	set rgb $i-capture.rgb
	save_rgb
	after 100
}


exit