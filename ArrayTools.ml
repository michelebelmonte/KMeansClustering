let safe_get m i j = 
  let i1 = max 0 i in
  let j1 = max 0 j in
  m.(i1).(j1)