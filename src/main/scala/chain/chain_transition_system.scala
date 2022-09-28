package chain

import core.*
import trees.ast.{AST, Scope, GlobalScopeMarker}

//////////////////////////////////////////////////
//// The semantics of a state transition system
//////////////////////////////////////////////////

trait ChainTransitionSystem[I[_]] extends Chain[I]:
  import TypeInference.given
  
  abstract class SystemSkeleton extends GlobalScopeMarker:
    protected def impl_premises(): I[Seq[I[Premise]]]
    lazy val premises = f["premises"] := fun0(impl_premises)
    
    // What is regarded as input for this system
    def System_Input_Binds: List[Any]
    
    // Binds that the system defines
    def System_Binds: List[Any]
    
    // Structs that the system defines
    def System_Structs: StructDefs

    def run_premises(): Either[I[Bool], I[PredicateError]] = ???
  end SystemSkeleton
end ChainTransitionSystem
