open Printf
open OUnit
open ListExt
open IterativeTools

module FloatClusterable:(Clusterable with type t=float)=struct
  type t=float

  let computeDistance (x:float) (y:float) = (x-.y)*.(x-.y)
  let computeCenter l=
    let sum = List.fold_left (fun state x -> state +. x) 0.0 l in
    sum /. (float_of_int (List.length l))
end

module FloatKMeansClusterable=ClusteringTools.Make(FloatClusterable)
                                       
module FloatClusteringIterativeRunner=IterativeTools.Make(FloatKMeansClusterable)

module FloatArrayClusterable:(Clusterable with type t=float array * int)=struct
  type t=float array * int

  let computeCenter l = 
        let sum = List.fold_left (fun state (x,_) -> state.(0) <- state.(0) +. x.(0);state.(1) <- state.(1) +. x.(1);state) [|0.0;0.0|] l in
        let length= List.length l in
        let x =sum.(0) /. (float_of_int length) in
        let y=sum.(1) /. (float_of_int length) in
        ([|x;y|],0)


  let computeDistance (arr0,_) (arr1,_) =
        let deltaX=arr0.(0)-. arr1.(0) in
        let deltaY=arr0.(1)-.arr1.(1) in
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
    | (arr,_)::tl ->
       begin
       printf "  Center: %f %f" arr.(0) arr.(1); print_newline ();
       printCenters tl;
       end
       
  let printCustomState state =
    let open FloatArrayClusteringIterativeRunner in
    printf "Iteration number %d" state.FloatArrayClusteringIterativeRunner.index; print_newline ();

    print_string "  Status: ";
    (match state.FloatArrayClusteringIterativeRunner.state with
     | Converged -> print_endline "Converged";
     | NonConverged -> print_endline "Non Converged";);

    FloatArrayKMeansClusterable.getCenters state.value |> printCenters;
    
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
       
