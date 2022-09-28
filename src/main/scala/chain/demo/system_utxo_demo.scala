package chain
package demo

import core.*
import trees.ast.{AST, ASTCore, RootEnv, Scope}
import chain.*
import trees.cgtree.CGTreeToScala
import trees.{CodeGenConfig, CodeGenHelpers, SourceCodeWriter}

trait UTxODemo[R[_]] extends UTxOSystem[R]/* with ChainWithDemoConfig[I]*/:
  import TypeInference.given

  def get_demo_txbody(): R[TxBody] =
    val slot_1: R[Slot] = slot_of_n(N_1)
    val txbody: R[TxBody] = txbody_new(slot_1)

    txbody
  end get_demo_txbody

  def get_demo_witness(): R[TxWitness] =
    val vkey_sig_map = map_new[VKey, Sig]()
    val scripthash_script_map = map_new[ScriptHash, Script]()
    val witness = TxWitness.Struct(vkey_sig_map, scripthash_script_map)

    witness
  end get_demo_witness

  def get_demo_metadata(): R[Metadata] =
    map_new[Nat, Metadatum]()
  end get_demo_metadata

  def get_demo_tx(): R[Tx] =
    //// Initial, not necessarily valid, data just to get the ball rolling
    val txbody = get_demo_txbody()
    val witness = get_demo_witness()
    val metadata = get_demo_metadata()
    val tx = Tx.Struct(txbody, witness, metadata)

    tx
  end get_demo_tx

  def get_demo_pparams(): R[PParams] =
    PParams.Struct(
      Z_1, // pparam_a
      Z_1, // pparam_b
    )
  end get_demo_pparams

  def get_demo_utxoenv(): R[UTxOEnv] =
    val slot = slot_of_n(nat(1))
    val pparams = get_demo_pparams()
    val pool_params: R[PoolParams] = map_new() // TODO abusing the map constructor, since the types match
    val genesis_delegs: R[GenesisDelegation] = map_new() // ditto

    val utxoenv = /*id["utxoenv"] := */UTxOEnv.Struct(slot, pparams, pool_params, genesis_delegs)

    utxoenv
  end get_demo_utxoenv

  def get_demo_utxostate() =
    val utxo: R[UTxO] = utxo_new_empty()
    val deposited: R[Coin] = coin_of_n(N_1)
    val fees: R[Coin] = coin_of_n(N_0)
    val pup: R[ProposedPPUpdates] = map_new()
    val fpup: R[ProposedPPUpdates] = map_new()
    val ppup: R[PPUpdateState] = PPUpdateState.Struct(pup, fpup)

    val res =
      UTxOState.Struct(
        utxo,
        deposited,
        fees,
        ppup
      )

    res
  end get_demo_utxostate

  // transaction that we need to handle
  val tx = get_demo_tx()
  // signal to activated the transition rules
  val signal = Signal_New_Tx

  val system = new UTxOSystem(
    impl_signal = signal,
    impl_tx = tx,
    impl_utxoenv = get_demo_utxoenv(),
    impl_utxostate = get_demo_utxostate()
  )
end UTxODemo

@main def utxo_demo: Unit =
  //val ast_core    = new ASTCore    with UTxODemo[AST]
  //val direct_core = new DirectCore with UTxODemo[Id]

  val ast_world    = new ASTSystemChain   (chain.demo.DemoChainConfig) with UTxODemo[AST] with ASTCore
  val direct_world = new DirectSystemChain(chain.demo.DemoChainConfig) with UTxODemo[Id]  with DirectCore

  val root_env = RootEnv()
  root_env.add_all(ast_world.Chain_BindTs)
  
  val system_env = root_env.new_child_env(Scope.GlobalScope(ast_world.system))
  val system_bind_ts = ast_world.system.System_Binds.map { case bind_t: AST.BindT[?] => bind_t }
  system_env.add_all(system_bind_ts)
  
  val writer =
    CodeGenHelpers.gen_source_code(
      ast_world.system.ubalance,
      system_env,
      CodeGenConfig(logger = Logger().set_logging_off()),
      new CGTreeToScala(),
      new SourceCodeWriter()
    )

  println(writer.get_code())
end utxo_demo

