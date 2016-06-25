open IterativeTools

module type Clusterable = sig
  type t

  val compute_distance: t -> t -> float
  val compute_center: t list -> (t option)
end
                            
module type CluterIterable=sig
  include Iterable

  type e
  val initialize: e list -> int -> t

  val get_clusters: t -> (e * e list) list
end

module Make(X:Clusterable):(CluterIterable with type e:=X.t)=struct
  open ListExt
  type  cluster = {
      center: X.t;
      totalDistance: float;
      items: X.t list
    }

  type t={clusters:cluster list; items:X.t list}

  let distFun=X.compute_distance
  let centerFun=X.compute_center

  let get_clusters v = List.map (fun x -> (x.center,x.items)) v.clusters
                              
  (* creates a cluster containig just the center.
items list is empty, and so totalDistance is 0.0*)
  let create_empty_cluster center =
    {center=center;totalDistance=0.0;items=[]}

(* adds an item to the cluter.
the items list is update, and the distance is added to the totalDistance*)
  let add_item_to_cluster {center;totalDistance;items} item distance =
    {center=center;totalDistance=totalDistance+.distance;items=item::items}


(* creates a list of empty clustes, given a list of centers*)
  let create_cluster_from_centers centers =
    List.map (fun x -> create_empty_cluster x) centers


(* assigns an item to a cluster that:
1. belongs to clusters
2. minimizes the distance between the item and the center of the cluster*)
let assign_item_to_cluster item clusters  = 
  let min= List.min (fun {center;_} -> distFun item center) clusters in
  match min with
    | None -> clusters
    | Some (cluster,distance) ->
      let cluster'=add_item_to_cluster cluster item distance in
      let clusters'= List.filter (fun x -> x <> cluster) clusters in
      cluster'::clusters'
    

(*assign the item l to the cluster whose center minimize the value of dist.
Return a new cluster list updated*)
let assign_items_to_clusters ~items clusters=
  List.fold_left (fun clusters x -> assign_item_to_cluster x clusters ) clusters items


(* creates k clusters given an items list.
the centers of the clusters are randomly selected from the items list.
items are assogned to the clusters so that the distFun is minimized.*)
let init items k =
  List.shuffle items
  |> List.take k
  |> create_cluster_from_centers


let get_overall_distance clusters =
  List.fold_left (fun state cluster -> state +. cluster.totalDistance) 0.0 clusters
    
(*given an items list create the initial clustering state*)
let initialize (items:X.t list) (k:int): t  =
  let clusters=init items k |> assign_items_to_clusters ~items in

  {clusters=clusters; items=items}

(* given a list l of items t produces k mean clusters*)
let get_next s =
      let clusters'=
        List.map (fun {center; totalDistance;items} -> centerFun items) s.clusters 
        |> List.fold_left (fun acc x -> match x with | Some c -> c::acc | None -> acc) []
        |> create_cluster_from_centers
        |> assign_items_to_clusters ~items:s.items in
      { s with clusters=clusters'}

let has_next prev current =
  get_overall_distance current.clusters < get_overall_distance prev.clusters
                                                           
end
