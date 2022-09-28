package chain

import core.*
import trees.*
import trees.ast.*
import trees.cgtree.*

class DirectChain(config: ChainConfig) extends ChainWithConfigParam[Id](config) with DirectCore

class DirectSystemChain(config: ChainConfig) extends ChainWithConfigParam[Id](config)
  with ChainTransitionSystem[Id]
  with DirectCore
