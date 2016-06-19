open IterativeTools

module type Clusterable = sig
  type t

  val computeDistance: t -> t -> float
  val computeCenter: t list -> t 
end
                            
module type CluterIterable=sig
  include Iterable

  type e
  val initialize: e list -> int -> t

  val getCenters: t -> e list
end

module Make(X:Clusterable):(CluterIterable with type e:=X.t)=struct
  open ListExt
  type  cluster = {
      center: X.t;
      totalDistance: float;
      items: X.t list
    }

  type t={clusters:cluster list; items:X.t list}

  let distFun=X.computeDistance
  let centerFun=X.computeCenter

  let getCenters v = List.map (fun x -> x.center) v.clusters
                              
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
let assign item clusters  = 
  let min= List.min (fun {center;_} -> distFun item center) clusters in
  match min with
    | None -> clusters
    | Some (cluster,distance) ->
      let cluster'=add cluster item distance in
      let clusters'= List.filter (fun x -> x <> cluster) clusters in
      cluster'::clusters'
    

(*assign the item l to the cluster whose center minimize the value of dist.
Return a new cluster list updated*)
let assignAll ~items clusters=
  List.fold_left (fun clusters x -> assign x clusters ) clusters items


(* creates k clusters given an items list.
the centers of the clusters are randomly selected from the items list.
items are assogned to the clusters so that the distFun is minimized.*)
let init items k =
  List.shuffle items
  |> List.take k
  |> create


let getOverallDistance clusters =
  List.fold_left (fun state cluster -> state +. cluster.totalDistance) 0.0 clusters
    
(*given an items list create the initial clustering state*)
let initialize (items:X.t list) (k:int): t  =
  let clusters=init items k |> assignAll ~items in

  {clusters=clusters; items=items}

(* given a list l of items t produces k mean clusters*)
let next s =
      let clusters'=
        List.map (fun {center; totalDistance;items} -> centerFun items) s.clusters
        |> create
        |> assignAll ~items:s.items in
      { s with clusters=clusters'}

let hasNext prev current =
  getOverallDistance current.clusters < getOverallDistance prev.clusters
                                                           
end
