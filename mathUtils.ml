
let min3 x y z =
  if (x < y) then
    if (x < z) then
      x
    else
      z
  else 
    if (y < z) then
      y
    else 
      z
