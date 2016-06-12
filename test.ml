open Printf
open OUnit

(*extend List with some utility functions*)
module List=struct
  include List

let shuffle l = 
  let nd = List.map (fun c -> (Random.bits (), c)) l in
  let sond = List.sort compare nd in
  List.map snd sond
  
let safeGet i j m =
  let i1 = max 0 i in
  let j1 = max 0 j in
  m.(i1).(j1)

let min costFun l =
  List.map (fun x-> (x,costFun x)) l |> List.fold_left (fun state (x,xcost) -> match state with
                                                       | None-> Some (x,xcost)
                                                       | Some (_,ycost) as state -> if (xcost<ycost) then Some (x,xcost) else state
                                       ) None

let max costFun l =
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

(*contains the implementation of the K Means Clustering algorithm*)
module KMeansClustering = struct
type  'a cluster = {
  center: 'a;
  totalDistance: float;
  items: 'a list
}

type numberOfIterations = int

type iterationStatus=Converged | NonConverged

type runningIteration = {status:iterationStatus; index:int}
                   
type iteration = Initial | Running of runningIteration

type 'a clusteringState = {
  iteration: iteration;
  clusters: ('a cluster) list;

  items: 'a list;
  distFun: 'a -> 'a -> float;
  centerFun: 'a list -> 'a
}

(* creates a cluster containig just the center.
items list is empty, and so totalDistance is 0.0*)
let create center =
  {center=center;totalDistance=0.0;items=[]}

(* adds an item to the cluter.
the items list is update, and the distance is added to the totalDistance*)
let add {center;totalDistance;items} item distance =
  {center=center;totalDistance=totalDistance+.distance;items=item::items}


(* creates a list of empty clustes, given a list of centers*)
let create centers =
  List.map (fun x -> create x) centers


(* assigns an item to a cluster that:
1. belongs to clusters
2. minimizes the distance between the item and the center of the cluster*)
let assign item clusters distFun = 
  let min= List.min (fun {center;_} -> distFun item center) clusters in
  match min with
    | None -> clusters
    | Some (cluster,distance) ->
      let cluster'=add cluster item distance in
      let clusters'= List.filter (fun x -> x <> cluster) clusters in
      cluster'::clusters'
    

(*assign the item l to the cluster whose center minimize the value of dist.
Return a new cluster list updated*)
let assignAll items clusters (distFun: 'a -> 'a -> float) =
  List.fold_left (fun clusters x -> assign x clusters distFun) clusters items


(* creates k clusters given an items list.
the centers of the clusters are randomly selected from the items list.
items are assogned to the clusters so that the distFun is minimized.*)
let init items k distFun =
  List.shuffle items
  |> List.take k
  |> create


let getOverallDistance clusters =
  List.fold_left (fun state cluster -> state +. cluster.totalDistance) 0.0 clusters

let getNextIndex status =
  ( match status with
        | Running {status=NonConverged;index}  ->
                  index +1;
        | Initial ->
                  1;
        | Running {status=Converged;_} -> assert false)
    
(*given an items list create the initial clustering state*)
let initialize items k distFun centerFun =
  let emptyClusters= init items k distFun in
  let clusters=assignAll items emptyClusters distFun in

  {iteration=Initial;clusters=clusters; items=items;distFun=distFun;centerFun=centerFun}


(* given a list l of items
                       t produces k mean clusters*)
let iterate state = 
  let iteration=state.iteration in

  match iteration with
  | Running  {status=Converged;_} -> failwith "clustering already converged"
    | Initial
    | Running  {status=NonConverged;_} ->
      let overallDistance=getOverallDistance state.clusters in

      let centers'= List.map (fun {center; totalDistance;items} -> state.centerFun items) state.clusters in

      let emptyClusters= create centers' in
      let clusters'= assignAll state.items emptyClusters state.distFun in

      let overallDistance'=getOverallDistance clusters' in

      let index' =getNextIndex iteration in
   
      if (overallDistance'>=overallDistance) then
        { state with iteration=Running {status=Converged;index=index'}}
       else 
        { state with iteration=Running {status=NonConverged;index=index'};clusters=clusters'}
      

end

module ClusterPrinting = struct
  let printCustomCluster cluster center_to_string =
    let open KMeansClustering in
    begin
      print_string "  Center: ";
      printf "  %s" (center_to_string cluster.center);
      print_newline ();
    end

   let printCluster cluster =
    printCustomCluster cluster (fun x -> sprintf "%f" x)

  let printCustomState state center_to_string=
    let open KMeansClustering in
    begin
   ( match state.iteration with 
    | Initial ->
       begin
         print_string "Initial status";
         print_newline ()
       end
         
    | Running {status;index} when status=NonConverged ->
       begin
         printf "Iteration: %d" index;
         print_newline ();
         print_string "  Status: intermediate";
         print_newline ()
       end
         
    | Running {status;index} when status=Converged ->
       begin
         printf "Iteration: %d" index;
         print_newline ();
         print_string "  Status: converged";
         print_newline ()
       end

   | Running _ -> assert false);
 
  print_string "  Total distance: ";
  print_float (getOverallDistance state.clusters);
  print_newline ();

  List.iter (fun x -> printCustomCluster x center_to_string ) state.clusters;
  print_newline ();
  print_newline ();
end
                           
  let printState state =
    printCustomState state (fun x -> sprintf "%f" x);

end

let () =
  let l=1::2::3::[] in
  let l'=List.take 2 l in
  OUnit.assert_bool "the list should have size equal to 2" (List.length l'==2)
                    


let () =
  let open KMeansClustering in
  let open ClusterPrinting in

  begin
      let centerFun l = 
  let sum = List.fold_left (fun state x -> state +. x) 0.0 l in
  sum /. (float_of_int (List.length l)) in


  let distFun (x:float) (y:float) = (x-.y)*.(x-.y) in
    print_endline "single cluster";
    let allValues=1.0::2.0::4.0::5.0::[] in
    let k=1 in

    let initialState=initialize allValues k distFun centerFun in
    begin
      printState initialState;

      let iter0= iterate initialState in

      printState iter0;

      let iter1= iterate iter0 in

      printState iter1;
    end
  end

    let () =
  let open KMeansClustering in
  let open ClusterPrinting in

  begin
      let centerFun l = 
  let sum = List.fold_left (fun state x -> state +. x) 0.0 l in
  sum /. (float_of_int (List.length l)) in


  let distFun (x:float) (y:float) = (x-.y)*.(x-.y) in
    print_endline "single cluster";
    let allValues=1.0::2.0::4.0::5.0::[] in
    let k=1 in

    let initialState=initialize allValues k distFun centerFun in
    begin
      printState initialState;

      let iter0= iterate initialState in

      printState iter0;

      let iter1= iterate iter0 in

      printState iter1;
    end
  end
    
  let () =
  let open KMeansClustering in
  let open ClusterPrinting in

  begin
      let centerFun l = 
  let sum = List.fold_left (fun state x -> state +. x) 0.0 l in
  sum /. (float_of_int (List.length l)) in


  let distFun (x:float) (y:float) = (x-.y)*.(x-.y) in
    print_endline "--------- double cluster --------";
    let allValues=1.0::2.0::4.0::50.0::51.0::52.0::40.0::[] in
    let k=2 in

    let initialState=initialize allValues k distFun centerFun in
    begin
      printState initialState;

      let iter0= iterate initialState in

      printState iter0;

      let iter1= iterate iter0 in

      printState iter1;
    end
  end

    
   let () =
  let open KMeansClustering in
  let open ClusterPrinting in
  let open Dataset in
  begin
    let rec loop state printfun iterToGo =
      if (iterToGo<0) then
        ()
      else
        let nextState=iterate state in
        begin
          printCustomState nextState printfun;
        match nextState.iteration with
        | Running {status;_} when status=Converged -> ()
        | _ -> loop nextState printfun (iterToGo-1)
        end in
    
      let centerFun l = 
        let sum = List.fold_left (fun state (x,_) -> state.(0) <- state.(0) +. x.(0);state.(1) <- state.(1) +. x.(1);state) [|0.0;0.0|] l in
        let length= List.length l in
        let x =sum.(0) /. (float_of_int length) in
        let y=sum.(1) /. (float_of_int length) in
        ([|x;y|],0) in


      let distFun (arr0,_) (arr1,_) =
        let deltaX=arr0.(0)-. arr1.(0) in
        let deltaY=arr0.(1)-.arr1.(1) in
        let squaredDistance=deltaX*.deltaX +.deltaY*.deltaY in
        sqrt squaredDistance
      in 

      let printCenter (arr,_)= sprintf "(%f,%f)" arr.(0) arr.(1) in
      
    print_endline "--------- large cluster --------";
    let k=3 in
    let l= Array.to_list samples in
    let initialState=initialize l  k distFun centerFun in
    begin
      printCustomState initialState printCenter;

      loop initialState printCenter 100;
    end
  end


   
