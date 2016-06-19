(*extend List with some utility functions*)
module List= struct
  include List

  let shuffle l = 
    let nd = List.map (fun c -> (Random.bits (), c)) l in
    let sond = List.sort compare nd in
    List.map snd sond
             
  let safeGet i j m =
    let i1 = max 0 i in
    let j1 = max 0 j in
    m.(i1).(j1)

  let min ~costFun l =
    List.map (fun x-> (x,costFun x)) l |> List.fold_left (fun state (x,xcost) -> match state with
                                                                                 | None-> Some (x,xcost)
                                                                                 | Some (_,ycost) as state -> if (xcost<ycost) then Some (x,xcost) else state
                                                         ) None

  let max ~costFun l =
    List.map (fun x -> (x,costFun x)) l |> List.fold_left (fun state (x,xcost) -> match state with
                                                                                  | None-> Some (x,xcost)
                                                                                  | Some (_,ycost) as state -> if (xcost>ycost) then Some (x,xcost) else state
                                                          ) None
                                                          
  let take k l =
    let rec doTake k l acc =
      if (k<1)then
        acc
      else
        match l with
        | [] -> acc
        | hd::tl -> doTake (k-1) tl (hd::acc)
    in
    doTake k l []
end
