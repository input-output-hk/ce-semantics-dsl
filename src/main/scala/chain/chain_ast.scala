package chain

import core.*
import trees.*
import trees.ast.*
import trees.cgtree.*

trait ASTChainWithBindTs extends Chain[AST]:
  def Chain_BindTs: BindTs = Chain_Binds.map { case bind_t: AST.BindT[?] => bind_t }
end ASTChainWithBindTs

class ASTChain(config: ChainConfig) extends ChainWithConfigParam[AST](config)
  with ASTChainWithBindTs
  with ASTCore

class ASTSystemChain(config: ChainConfig) extends ChainWithConfigParam[AST](config)
  with ASTChainWithBindTs
  with ChainTransitionSystem[AST]
  with ASTCore
