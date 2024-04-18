if { $::argc > 2 } {
    set dcdfile [lindex $argv 0]
    puts "dcdfile: $dcdfile"
    set i [lindex $argv 1]
    puts "i: $i"
    set f [lindex $argv 2]
    puts "f: $f"
} else {
    puts "not enough command line argument passed"
}

animate read dcd $dcdfile waitfor all
puts "$dcdfile was loaded"

puts "writing to f${i}to${f}.dcd"
animate write dcd "f${i}to${f}.dcd" beg $i end [expr $f-1] waitfor all

exit
