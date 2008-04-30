let pre r f =
  let old = !r in
    r := f old;
    old

let post r f =
  r := f !r;
  !r

let pre_incr  r = pre  r ( ( + ) 1 )
let pre_decr  r = pre  r ( ( - ) 1 )
let post_incr r = post r ( ( + ) 1 )
let post_decr r = post r ( ( - ) 1 )
