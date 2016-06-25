open Printf
open OUnit
open ListExt
open IterativeTools

module FloatClusterable:(ClusteringTools.Clusterable with type t=float)=struct
  type t=float

  let compute_distance (x:float) (y:float) = (x-.y)*.(x-.y)
  let compute_center l=
    let sum = List.fold_left (fun state x -> state +. x) 0.0 l in
    Some (sum /. (float_of_int (List.length l)))
end

module FloatKMeansClusterable=ClusteringTools.Make(FloatClusterable)
                                       
module FloatClusteringIterativeRunner=IterativeTools.Make(FloatKMeansClusterable)

module FloatArrayClusterable:(ClusteringTools.Clusterable with type t=float array * int)=struct
  type t=float array * int

  let compute_center l = 
        let sum = List.fold_left (fun state (x,_) -> state.(0) <- state.(0) +. x.(0);state.(1) <- state.(1) +. x.(1);state) [|0.0;0.0|] l in
        let length = List.length l in
        let x = sum.(0) /. (float_of_int length) in
        let y = sum.(1) /. (float_of_int length) in
        Some ([|x;y|],0)


  let compute_distance (arr0,_) (arr1,_) =
        let deltaX=arr0.(0)-. arr1.(0) in
        let deltaY=arr0.(1)-. arr1.(1) in
        let squaredDistance=deltaX*.deltaX +.deltaY*.deltaY in
        sqrt squaredDistance
             
end

module FloatArrayKMeansClusterable=ClusteringTools.Make(FloatArrayClusterable)
                                       
module FloatArrayClusteringIterativeRunner=IterativeTools.Make(FloatArrayKMeansClusterable)

(*---------*)
module ClusterPrinting = struct
  let rec printCenters l =
    match l with
    | [] -> ()
    | ((arr,_),items)::tl ->
       begin
       printf "  Center of %d items: %f %f" (List.length items) arr.(0) arr.(1); print_newline ();
       printCenters tl;
       end
       
  let printCustomState state =
    let open FloatArrayClusteringIterativeRunner in
    printf "Iteration number %d" state.FloatArrayClusteringIterativeRunner.index; print_newline ();

    print_string "  Status: ";
    (match state.FloatArrayClusteringIterativeRunner.state with
     | Converged -> print_endline "Converged";
     | NonConverged -> print_endline "Non Converged";);

    FloatArrayKMeansClusterable.get_clusters state.value |> printCenters;
    
end

(*-----*)
let ()=
  let c0=FloatKMeansClusterable.initialize (1.0::2.0::[]) 1 in
  let v0=FloatClusteringIterativeRunner.init c0 in
  let v1=FloatClusteringIterativeRunner.iterate v0 in
  let open FloatKMeansClusterable in
  begin
    
  print_int v1.FloatClusteringIterativeRunner.index;
 
  end
            
let () =
  let open ClusterPrinting in

  begin
    print_string "single cluster";
    print_newline ();
    let c0=FloatKMeansClusterable.initialize (1.0::2.0::4.0::5.0::[]) 1 in
    let v0=FloatClusteringIterativeRunner.init c0 in
    let v1=FloatClusteringIterativeRunner.iterate v0 in
    let v2=FloatClusteringIterativeRunner.iterate v1 in
    print_int v2.FloatClusteringIterativeRunner.index;
  end
    
  let () =
  let open ClusterPrinting in

  begin
    print_endline "--------- double cluster --------";
    let allValues=1.0::2.0::4.0::50.0::51.0::52.0::40.0::[] in
    let k=2 in
    let c0=FloatKMeansClusterable.initialize allValues k in
    let v0=FloatClusteringIterativeRunner.init c0 in
    let v1=FloatClusteringIterativeRunner.iterate v0 in
    let v2=FloatClusteringIterativeRunner.iterate v1 in
    print_int v2.FloatClusteringIterativeRunner.index;
  end

    
let () =
  let open ClusterPrinting in
  let open Dataset in
  begin
    let rec loop state iterToGo =
      if (iterToGo<0) then
        ()
      else
        let nextState=FloatArrayClusteringIterativeRunner.iterate state in
        begin
          printCustomState nextState;
        match nextState.FloatArrayClusteringIterativeRunner.state with
        | FloatArrayClusteringIterativeRunner.Converged -> ()
        | FloatArrayClusteringIterativeRunner.NonConverged -> loop nextState (iterToGo-1)
        end in
      
    print_endline "--------- large cluster --------";
    let k=3 in
    let l= Array.to_list samples in
    let c0=FloatArrayKMeansClusterable.initialize l k in
    let v0=FloatArrayClusteringIterativeRunner.init c0 in
    begin
      printCustomState v0;
      loop v0 100;
    end
  end

module DtwClusterable=ClusteringTools.Make(DinamicTimeWrap)

module DtwIterable=IterativeTools.Make(DtwClusterable)

module DtwClusterPrinting = struct
  let rec printCenters l =
    match l with
    | [] -> ()
    | (arr,items)::tl ->
       begin
         printf "  Center of %d items: " (List.length items);
         List.iter (fun x -> printf "%f, " x) arr;
         print_newline ();
         printCenters tl;
       end
       
  let printCustomState state =
    let open DtwIterable in
    printf "Iteration number %d" state.DtwIterable.index; print_newline ();

    print_string "  Status: ";
    (match state.DtwIterable.state with
     | Converged -> print_endline "Converged";
     | NonConverged -> print_endline "Non Converged";);

    DtwClusterable.get_clusters state.value |> printCenters;
    
end
                                      
let () =
  let rec loop state iterToGo =
      if (iterToGo<0) then
        ()
      else
        let nextState=DtwIterable.iterate state in
        begin
          DtwClusterPrinting.printCustomState nextState;
        match nextState.DtwIterable.state with
        | DtwIterable.Converged -> ()
        | DtwIterable.NonConverged -> loop nextState (iterToGo-1)
        end in
  let values = [[1.0;2.0;3.0;2.0;1.0;1.0];
                [1.0;1.0;1.0;20.0;1.0;1.0];
                [1.0;1.0;2.0;3.0;2.0;1.0];
                [2.0;3.0;2.0;1.0;1.0];
                [1.0;1.0;1.0;21.0;1.0;1.0];
                [1.0;1.0;1.0;18.0;1.0;1.0];
               ] in
  let k = 2 in
  let c0=DtwClusterable.initialize values k in
  let v0=DtwIterable.init c0 in
  DtwClusterPrinting.printCustomState v0;
  loop v0 100;
