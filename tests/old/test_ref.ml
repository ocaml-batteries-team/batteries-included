(*Testing our linking problem with module [Ref]*)

let x = ref 0;;

Data.Mutable.Ref.pre_incr x;;

print_int !x;;
