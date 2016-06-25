type t= float list

let pam l compute_distance =
  let open ListExt in
  let rec doPam tsAll tsRemaining compute_distance best =
    match tsRemaining with
    | [] ->
      (
        match best with
        | None -> None
        | Some (ts, _) -> Some ts
      )
    | hd::tl ->
      let c' = List.fold_left (fun c ts -> c +. compute_distance hd ts) 0.0 tsAll in
      (
      match best with
      | None -> doPam tsAll tl compute_distance (Some (hd, c'))
      | Some (ts, c) ->
        if (c' > c) then
          Some ts
        else 
          doPam tsAll tl compute_distance (Some (hd, c')) 
        ) in
  match l with
  | [] -> None
  | [x] -> Some x
  | _ as l ->
    let randomList = List.shuffle l in
    doPam l randomList compute_distance None

let compute_distance t0 t1 =
  let n = List.length t0 in
  let m = List.length t1 in
  let infiniteValue = 1000000000.0 in
  let dtw = Array.make_matrix n m 0.0 in
  begin
    (*initialize column 0 with infinite values*)
    List.iteri (fun i x -> dtw.(i).(0) <- infiniteValue) t0;
    (*initialize row 0 with infinite values*)
    List.iteri (fun j x -> dtw.(0).(j) <- infiniteValue) t1;
    (*reset (0,0) to 0*)
    dtw.(0).(0) <- 0.0;
    (*compute costs*)
    List.iteri
      (
        fun i x ->
          List.iteri
            (
              fun j y -> 
                (*cost of the current step*)
                let cost = abs_float (x -. y) in
                (*cost to arrive to the currest step =
                  1. cost of the currect step
                  plus
                  2. min cost to arrive in one of the 3 possible previous steps*)
                dtw.(i).(j) <- cost +.
                                MathUtils.min3
                                  (ArrayTools.safe_get dtw (i - 1) (j - 1))
                                  (ArrayTools.safe_get dtw i (j - 1))
                                  (ArrayTools.safe_get dtw (i - 1) j)
            )
            t1
      )
      t0;
    dtw.(n - 1).(m - 1);
  end

let compute_center l =
  pam l compute_distance
