if { $::argc > 3 } {
    set pdbname [lindex $argv 0]
    puts "pdbname: $pdbname"
    set theta [lindex $argv 1]
    puts "theta: $theta"
    set phi [lindex $argv 2]
    puts "phi: $phi"
    set psi [lindex $argv 3]
    puts "psi: $psi"
} else {
    puts "not enough command line argument passed"
    exit
}

mol new $pdbname

#Define rotation matrices for theta (around y-axis) and phi (around z-axis)
set thetamatrix [transaxis y $theta]
set phimatrix [transaxis z $phi]
# Define rotation matrix for psi being rotation around the pointing direction defined by v
set ez {0.0 0.0 1.0}
set v [vectrans $phimatrix [vectrans $thetamatrix $ez]]
puts "v-direction: $v"
set psimatrix [transabout $v $psi]

#select protein atoms
set sel [atomselect top "all"]
#Translate center of mass to origin
set com [measure center $sel weight mass]
$sel moveby [vecscale -1.0 $com]

#Apply Rotations
$sel move $thetamatrix
$sel move $phimatrix
$sel move $psimatrix

#Write output
puts "writing to theta${theta}_phi${phi}_psi${psi}.dcd"
animate write dcd "theta${theta}_phi${phi}_psi${psi}.dcd" beg 0 end 0 waitfor all
exit

