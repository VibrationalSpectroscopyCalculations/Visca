#V2 with automatic input

if { $::argc > 2 } {
    set dcdfile [lindex $argv 0]
    puts "dcdfile: $dcdfile"
    set steps [lindex $argv 1]
    puts "step: $steps"
    set fperstep [lindex $argv 2]
    puts "fperstep: $fperstep"
} else {
    puts "not enough command line argument passed"
}
#set dcdfile "asn_a99SBdisp_s2_r1.dcd"

#mol new $pdbfile
animate read dcd $dcdfile waitfor all
puts "$dcdfile was loaded"

#set steps 8
#set fperstep 1000

for {set i 0} {$i < $steps} {incr i} {
set ii [expr $i*$fperstep]
set jj [expr $ii+$fperstep]
puts "writing to f${ii}to${jj}.dcd"
animate write dcd "f${ii}to${jj}.dcd" beg $ii end [expr $jj-1] waitfor all
}

#[atomselect top all] writedcd test0000to1000.dcd

exit
