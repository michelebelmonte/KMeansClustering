                           
module type Iterable = sig
  type t
 
  val get_next: t -> t
  val has_next: t -> t -> bool
end
                                                                                 
module type IterationRunner=sig
  type t

  type state= NonConverged | Converged
                               
  type iteration ={state:state; index:int;value:t}
                    
  val init: t -> iteration
  val iterate: iteration -> iteration
end

module Make(X:Iterable):(IterationRunner with type t:=X.t) = struct
    type state= NonConverged | Converged
                               
    type iteration ={state:state; index:int;value:X.t}

    let next=X.get_next
    let hasNext=X.has_next
                  
    let init v ={state=NonConverged;index=0;value=v}
                  
    let iterate i =
      match i.state with
      | Converged -> failwith "Cannot iterate when already converged"
      | NonConverged -> let index'=i.index+1 in
                        let value'= next i.value in
                        if (hasNext i.value value') then
                          {state=NonConverged;index=index';value=value'}
                        else
                          {state=Converged;index=index';value=value'}
end
