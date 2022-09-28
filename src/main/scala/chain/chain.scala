package chain

import core.*
import trees.ast.*

import scala.annotation.targetName

// Anything related to the definition of a chain, built on top of the core language.
// When we say "chain" we mean Ledger and other potentially building blocks.
// The page and figure references are from the Cardano Ledger PDF Spec.
trait Chain[R[_]] extends Core[R]:
  import TypeInference.given

  /////////////////////////////////////////////////////////
  // Core definitions
  /////////////////////////////////////////////////////////

  // fig.2, p.5
  // private signing key
  type SKey = String

  // Public verifying key
  type VKey = String

  // Hash of a key
  type KeyHash = String
  type KeyHash_VRF  = KeyHash
  type KeyHash_Pool = KeyHash
  type KeyHash_G    = KeyHash

  // Signature
  type Sig = String

  // multi-signature script
  type Script = String
  // Hash of a script
  type ScriptHash = String

  // Index
  type Ix = Nat // p.10 ledger-spec
  
  // fig.12, p.19 ledger-spec
  // genesis delegations
  type GenesisDelegation = Map[R[KeyHash_G], R[Pair[R[KeyHash], R[KeyHash_VRF]]]]

  // fig.7, p.13 ledger-spec
  type ProtVer = Pair[R[Nat] /* m */, R[Nat] /* n */] // protocol version

  object bf: // Bare fields
    // Use `inline` here and watch Scala die (we get just a runtime error instead of compiler death)
    // Note Apparently, `@inline` instead of `inline` is fine :shrug:
    private def F[N <: FieldName: ValueOf, F: Ty]: BareField[N, F] = bare_field[N, F]

    val v_Nat = F["v", Nat]
    val v_Zat = F["v", Zat]
    val v_String = F["v", String]

    val ttl = F["ttl", Slot]

    // PParams, p.13, fig.7
    val a = F["a", Zat]
    val b = F["b", Zat]
    val max_block_size  = F["max_block_size", Nat]
    val max_tx_size     = F["max_tx_size", Nat]
    val max_header_size = F["max_header_size", Nat]
    val pool_deposit = F["pool_deposit", Coin]
    val E_max = F["E_max", Epoch]
    val n_opt = F["n_opt", Nat] // TODO Actually the type should be like: PositiveNat = Nat - {0}
    //val a_0 = F["a_0", NonNegativeReal]
    // ...
    val min_UTxO_value = F["min_UTxO_value", Coin]
    val min_pool_cost  = F["min_pool_cost", Coin]


    val txid = F["txid", TxId]
    val ix   = F["ix", Ix]

    val addr  = F["addr", Addr]
    val coins = F["coins", Coin]

    val txbody    = F["body",     TxBody]
    val txwitness = F["witness",  TxWitness]
    val metadata  = F["metadata", Metadata]

    val vkey_sig_map          = F["vkey_sig_map",          Map[R[VKey], R[Sig]]]
    val scripthash_script_map = F["scripthash_script_map", Map[R[ScriptHash], R[Script]]]

    val pup  = F["pup", ProposedPPUpdates]
    val fpup = F["fpup", ProposedPPUpdates]

    val slot = F["slot", Slot]
    val pparams = F["pparams", PParams]
    val genesis_delegs = F["genesis_delegs", GenesisDelegation]

    val pool_params = F["pool_params", PoolParams]

    val utxo = F["utxo", UTxO]
    val deposited = F["deposited", Coin]
    val fees = F["fees", Coin]
    val ppup = F["ppup", PPUpdateState]

    val url = F["url", Url]
    val pool_MD_hash = F["pool_MD_hash", PoolMDHash]
  end bf

  // fig.7, p.13 ledger-spec
  // KES period
  object KESPeriod {
    case class KESPeriodStruct(v: R[Nat]) extends AnyStruct
    val Fields = Tuple1(bf.v_Nat)
    val Constr = struct_constructor[KESPeriodStruct]

    val Struct = def_struct(Fields, Constr)
    val v      = def_field(Struct, bf.v_Nat, _.v)
  }
  type KESPeriod = KESPeriod.KESPeriodStruct

  // fig.7, p.13 ledger-spec
  object Epoch {
    case class EpochStruct(v: R[Nat]) extends AnyStruct
    val Fields = Tuple1(bf.v_Nat)
    val Constr = struct_constructor[EpochStruct]

    val Struct = def_struct(Fields, Constr)
    val v      = def_field(Struct, bf.v_Nat, _.v)
  }
  type Epoch = Epoch.EpochStruct

  // fig.7, p.13 ledger-spec
  object Coin {
    case class CoinStruct(v: R[Zat]) extends AnyStruct
    val Fields = Tuple1(bf.v_Zat)
    val Constr = struct_constructor[CoinStruct]

    val Struct = def_struct(Fields, Constr)
    val v      = def_field(Struct, bf.v_Zat, _.v)
  }
  type Coin = Coin.CoinStruct

  // Absolute slot
  // p.10 ledger-spec
  object Slot {
    case class SlotStruct(v: R[Nat]) extends AnyStruct
    val Fields = Tuple1(bf.v_Nat)
    val Constr = struct_constructor[SlotStruct]

    val Struct = def_struct(Fields, Constr)
    val v      = def_field(Struct, bf.v_Nat, _.v)
  }
  type Slot = Slot.SlotStruct

  // fig.7, p.13 ledger-spec
  object Duration {
    case class DurationStruct(v: R[Nat]) extends AnyStruct
    val Fields = Tuple1(bf.v_Nat)
    val Constr = struct_constructor[DurationStruct]

    val Struct = def_struct(Fields, Constr)
    val v      = def_field(Struct, bf.v_Nat, _.v)
  }
  type Duration = Duration.DurationStruct

  // fig.10, p.16 ledger-spec
  // reward withdrawal: maps a reward address to the coin value to be withdrawn
  type Wdrl = Map[R[Addr], R[Coin]]

  //enum Network:
  //  case Mainnet
  //  case Testnet
  //end Network
  //
  //enum Signal:
  //  case SignalTx
  //end Signal

  // Address.
  object Addr:
    case class AddrStruct(v: R[String]) extends AnyStruct
    val Fields = Tuple1(bf.v_String)
    val Constr = struct_constructor[AddrStruct]

    val Struct = def_struct(Fields, Constr)
    val v      = def_field(Struct, bf.v_String, _.v)
  end Addr

  type Addr = Addr.AddrStruct
  type Addr_Rwd = Addr

  // f.21, p.32 ledger-spec
  // Also described in p. 30 (Delegation definitions)
  type DecimalIn01 = Double // decimal in [0,1]
  opaque type Url = String
  opaque type PoolMDHash = String
  object PoolMD {
    case class PoolMDStruct(url: R[Url], pool_MD_hash: R[PoolMDHash]) extends AnyStruct
    val Fields = (bf.url, bf.pool_MD_hash)
    val Constr = struct_constructor[PoolMDStruct]

    val Struct = def_struct(Fields, Constr)
    val url    = def_field(Struct, bf.url, _.url)
    val pool_MD_hash = def_field(Struct, bf.pool_MD_hash, _.pool_MD_hash)
  }
  type PoolMD = PoolMD.PoolMDStruct
  type PoolParam = String //(I[Set[KeyHash]], I[Coin], I[DecimalIn01], I[Coin], I[Addr_Rwd], I[KeyHash_VRF], I[Url], I[PoolMD]) // TODO we do not have tuples yet
  type PoolParams = Map[R[KeyHash_Pool], R[PoolParam]]

  /////////////////////////////////////////////////////////
  // Transactions
  /////////////////////////////////////////////////////////

  // Transaction Id
  type TxId = String // fig.10, p.16 ledger-spec

  // fig.10, p.16 ledger-spec
  // TxIn = TxId × Ix
  object TxIn {
    case class TxInStruct(txid: R[TxId], ix: R[Ix]) extends AnyStruct
    val Fields: (BareField["txid", TxId], BareField["ix", Ix]) = (bf.txid, bf.ix)
    val Constr = struct_constructor[TxInStruct]

    val Struct = def_struct(Fields, Constr)
    val txid   = def_field(Struct, bf.txid, _.txid)
    val ix     = def_field(Struct, bf.ix,   _.ix)
  }
  type TxIn = TxIn.TxInStruct

  // fig.10, p.16 ledger-spec
  // TxOut = Addr × Coin
  object TxOut {
    case class TxOutStruct(addr: R[Addr], coins: R[Coin]) extends AnyStruct
    val Fields = (bf.addr, bf.coins)
    val Constr = struct_constructor[TxOutStruct]

    val Struct = def_struct(Fields, Constr)
    val addr   = def_field(Struct, bf.addr,  _.addr)
    val coins  = def_field(Struct, bf.coins, _.coins)
  }
  type TxOut = TxOut.TxOutStruct

  // sec.6, p.15:
  // "The UTxO type will be used by the ledger state to store all the unspent transaction
  // outputs. It is a finite map from transaction inputs to transaction outputs that are
  // available to be spent."
  //case class UTxO(utxo: I[Map[TxIn, TxOut]]) // fig.10, p.16 ledger-spec
  // UTxO = TxIn ↦ TxOut
  type UTxO = Map[R[TxIn], R[TxOut]]

  // Sec.6, p.15
  // P TxIn × (Ix ↦ TxOut) × DCert∗ × Coin × Slot × Wdrl × Update? × MetadataHash?

  object TxBody {
    case class TxBodyStruct(ttl: R[Slot]) extends AnyStruct
    val Fields = Tuple1(bf.ttl)
    val Constr = struct_constructor[TxBodyStruct]

    val Struct = def_struct(Fields, Constr)
    val ttl    = def_field(Struct, bf.ttl, _.ttl)
  }
  type TxBody = TxBody.TxBodyStruct

  // fig.10, p.16
  // (VKey ↦ Sig) x (ScriptHash ↦ Script)
  object TxWitness {
    case class TxWitnessStruct(
      vkey_sig_map: R[Map[R[VKey], R[Sig]]],
      scripthash_script_map: R[Map[R[ScriptHash], R[Script]]]
    ) extends AnyStruct
    val Fields = (bf.vkey_sig_map, bf.scripthash_script_map)
    val Constr = struct_constructor[TxWitnessStruct]

    val Struct = def_struct(Fields, Constr)
    val vkey_sig_map          = def_field(Struct, bf.vkey_sig_map,          _.vkey_sig_map)
    val scripthash_script_map = def_field(Struct, bf.scripthash_script_map, _.scripthash_script_map)
  }
  type TxWitness = TxWitness.TxWitnessStruct

  type Metadatum = String
  type Metadata = Map[R[Nat], R[Metadatum]]

  // TxBody x TxWitness x Metadata?
  object Tx {
    case class TxStruct(body: R[TxBody], witness: R[TxWitness], metadata: R[Metadata]) extends AnyStruct
    val Fields = (bf.txbody, bf.txwitness)
    val Constr = struct_constructor[TxStruct]

    val Struct = def_struct(Fields, Constr)
    val body     = def_field(Struct, bf.txbody,    _.body)
    val witness  = def_field(Struct, bf.txwitness, _.witness)
    val metadata = def_field(Struct, bf.metadata,  _.metadata)
  }
  type Tx = Tx.TxStruct

  /////////////////////////////////////////////////////////
  // Rules
  /////////////////////////////////////////////////////////

  // A predicate is something that computes to a Bool
  type Predicate = Fun0[R, Bool]
  type PredicateError = String // Note could use a native (=metalanguage) enum and reuse it in all semantics
  type Premise = Pair[R[Predicate], R[PredicateError]]
  def new_predicate_error(s: String): R[PredicateError] = str(s)

  type Network = String
  def new_network(s: String): R[Network] = str(s)

  type Signal = String
  def new_signal(s: String): R[Signal] = str(s)
  
  /////////////////////////////////////////////////////////
  // Environments and States
  /////////////////////////////////////////////////////////

  // protocol parameter update state
  object PPUpdateState {
    case class PPUpdateStateStruct(
      pup: R[ProposedPPUpdates], // current proposals
      fpup: R[ProposedPPUpdates] // future proposals
    ) extends AnyStruct

    val Fields = (bf.pup, bf.fpup)
    val Constr = struct_constructor[PPUpdateStateStruct]

    val Struct = def_struct(Fields, Constr)
    val pup    = def_field(Struct, bf.pup,  _.pup)
    val fpup   = def_field(Struct, bf.fpup, _.fpup)
  }
  type PPUpdateState = PPUpdateState.PPUpdateStateStruct

  object PPUpdateEnv {
    case class PPUpdateEnvStruct(
      slot: R[Slot],  // current slot
      pparams: R[PParams], // protocol parameters
      genesis_delegs: R[GenesisDelegation] // genesis key delegations
    ) extends AnyStruct

    val Fields = (bf.slot, bf.pparams, bf.genesis_delegs)
    val Constr = struct_constructor[PPUpdateEnv]

    val Struct = def_struct(Fields, Constr)
    val slot    = def_field(Struct, bf.slot,    _.slot)
    val pparams = def_field(Struct, bf.pparams, _.pparams)
    val genesis_delegs = def_field(Struct, bf.genesis_delegs, _.genesis_delegs)
  }
  type PPUpdateEnv = PPUpdateEnv.PPUpdateEnvStruct

  // fig.15, p.23 ledger-spec
  // UTxO Environment

  object UTxOEnv {
    case class UTxOEnvStruct(
      slot: R[Slot],              // current slot
      pparams: R[PParams],        // protocol parameters
      pool_params: R[PoolParams], // stake pools
      genesis_delegs: R[GenesisDelegation] // genesis key delegations
    ) extends AnyStruct

    val Fields = (bf.slot, bf.pparams, bf.pool_params, bf.genesis_delegs)
    val Constr = struct_constructor[UTxOEnvStruct]

    val Struct = def_struct(Fields, Constr)
    val slot    = def_field(Struct, bf.slot,    _.slot)
    val pparams = def_field(Struct, bf.pparams, _.pparams)
    val pool_params    = def_field(Struct, bf.pool_params,    _.pool_params)
    val genesis_delegs = def_field(Struct, bf.genesis_delegs, _.genesis_delegs)
  }
  type UTxOEnv = UTxOEnv.UTxOEnvStruct

  // fig.15, p.23 ledger-spec
  // UTxO States
  object UTxOState {
    case class UTxOStateStruct(
      utxo: R[UTxO],
      deposited: R[Coin],    // deposit pot
      fees: R[Coin],         // fee pot
      ppup: R[PPUpdateState] // proposed updates
    ) extends AnyStruct

    val Fields = (bf.utxo, bf.deposited, bf.fees, bf.ppup)
    val Constr = struct_constructor[UTxOStateStruct]

    val Struct    = def_struct(Fields, Constr)
    val utxo      = def_field(Struct, bf.utxo,      _.utxo)
    val deposited = def_field(Struct, bf.deposited, _.deposited)
    val fees      = def_field(Struct, bf.fees,      _.fees)
    val ppup      = def_field(Struct, bf.ppup,      _.ppup)
  }
  type UTxOState = UTxOState.UTxOStateStruct

  /////////////////////////////////////////////////////////
  // Protocol Parameters
  /////////////////////////////////////////////////////////

  // fig.7, p.13 ledger-spec
  // protocol parameters

  object PParams {
    case class PParamsStruct(
      a: R[Zat],
      b: R[Zat],
    ) extends AnyStruct

    val Fields = (bf.a, bf.b)
    val Constr = struct_constructor[PParamsStruct]

    val Struct = def_struct(Fields, Constr)
    val a = def_field(Struct, bf.a, _.a)
    val b = def_field(Struct, bf.b, _.b)

    // max block body size: I[Nat]
    // max transaction size: I[Nat]
    // max block header size: I[Nat]
    // stake pool deposit: I[Coin]
    // protocol version: I[ProtVer]
    // minimum allowed value of a new UTxOut: I[Coin]
  }

  type PParams = PParams.PParamsStruct
  type PParamsUpdate = PParams

  // fig.10, p.16 ledger-spec
  // proposed updates
  type ProposedPPUpdates = Map[R[KeyHash_G], R[PParamsUpdate]]
  // update proposal
  type Update = Pair[R[ProposedPPUpdates], R[Epoch]]
  
  protected def impl_kes_period_of_n(n: R[Nat]): R[KESPeriod] = KESPeriod.Struct(Tuple1(n))

  lazy val kes_period_of_n = f["kes_period_of_n"] := p["n"] --> impl_kes_period_of_n

  // fig.7, p.13 ledger-spec
  protected def impl_epoch_of_n(n: R[Nat]): R[Epoch] = Epoch.Struct(Tuple1(n))

  lazy val epoch_of_n = f["epoch_of_n"] := p["n"] --> impl_epoch_of_n

  protected def impl_n_of_epoch(e: R[Epoch]): R[Nat] = e.get(Epoch.v)

  lazy val n_of_epoch = f["n_of_epoch"] := p["e"] --> impl_n_of_epoch


  // fig.7, p.13 ledger-spec
  // See also p.11, sec.5.1
  // Type transformation.
  // TODO What are the failure conditions here???
  // TODO Opportunity for tests?
  // TODO Opportunity for pre- and post- conditions?
  protected def impl_coin_of_z(z: R[Zat]): R[Coin] = Coin.Struct(Tuple1(z))

  lazy val coin_of_z = f["coin_of_z"] := p["z"] --> impl_coin_of_z

  protected def impl_coin_of_n(n: R[Nat]): R[Coin] =
    coin_of_z(zat_of_nat(n))
  end impl_coin_of_n

  lazy val coin_of_n = f["coin_of_n"] := p["n"] --> impl_coin_of_n

  lazy val coin_zero = f["coin_zero"] := impl_coin_of_n(N_0)

  protected def impl_coin_add(a: R[Coin], b: R[Coin]): R[Coin] =
    Coin.Struct(Tuple1(a.get(Coin.v) + b.get(Coin.v)))
  end impl_coin_add

  lazy val coin_add = f["coin_add"] := (p["a"], p["b"]) --> impl_coin_add

  //// Absolute slot
  // p.10 ledger-spec
  inline def n_of_slot(s: R[Slot]): R[Nat] = s.get(Slot.v)

  inline def slot_of_n(n: R[Nat]): R[Slot] = Slot.Struct(Tuple1(n))

  protected def impl_slot_gte(a: R[Slot], b: R[Slot]): R[Bool] =
    a.get(Slot.v) >= b.get(Slot.v)
  end impl_slot_gte

  lazy val slot_gte = f["slot_gte"] := (p["a"], p["b"]) --> impl_slot_gte

  extension(a: R[Slot])
    @targetName("ext_a_slot_gte_b__")
    def >=(b: R[Slot]): R[Bool] = impl_slot_gte(a, b)
  end extension


  // fig.7, p.13 ledger-spec
  // difference between slots
  inline def duration_of_nat(nat: R[Nat]): R[Duration] = Duration.Struct(Tuple1(nat))

  // fig.7, p.13
  protected def impl_duration_of_slots(a: R[Slot], b: R[Slot]): R[Duration] =
    val n_a  = v["n_a"]  := n_of_slot(a)
    val n_b  = v["n_b"]  := n_of_slot(b)
    val diff = v["diff"] := n_b - n_a

    Duration.Struct(Tuple1(diff))
  end impl_duration_of_slots

  lazy val duration_of_slots = f["duration_of_slots"] := (p["a"], p["b"]) --> impl_duration_of_slots

  //// fig.10, p.16 ledger-spec
  //// reward withdrawal: maps a reward address to the coin value to be withdrawn
  //type Wdrl = Map[I[Addr], I[Coin]]

  lazy val Networkid_mainnet = v["Networkid_mainnet"] := new_network("mainnet")

  lazy val Networkid_testnet = v["Networkid_testnet"] := new_network("testnet")

  lazy val Signal_New_Tx = v["Signal_New_Tx"] := new_signal("Signal_New_Tx")

  protected def impl_utxo_new_empty(): R[UTxO] =
    val a: R[UTxO] = v["a"] := map_new()
    a
  end impl_utxo_new_empty

  lazy val utxo_new_empty = f["utxo_new_empty"] := fun0(impl_utxo_new_empty)

  //// Sec.6, p.15
  //// P TxIn × (Ix ↦ TxOut) × DCert∗ × Coin × Slot × Wdrl × Update? × MetadataHash?
  protected def impl_txbody_new(ttl: R[Slot]): R[TxBody] = TxBody.Struct(Tuple1(ttl))

  lazy val txbody_new = f["txbody_new"] := p["slot"] --> impl_txbody_new

  // Accessor ttl: Slot
  protected def impl_txbody_get_ttl(txbody: R[TxBody]): R[Slot] = txbody.get(TxBody.ttl)

  lazy val txbody_get_ttl = f["txbody_get_ttl"] := p["txbody"] --> impl_txbody_get_ttl
  
  // Accessor txins: Set[TxIn]
  //  transaction inputs
  protected def impl_txbody_get_txins(txbody: R[TxBody]): R[Set[R[TxIn]]] =
    // TODO implement
    set_new()
  end impl_txbody_get_txins

  lazy val txbody_get_txins = f["txbody_get_txins"] := p["txbody"] --> impl_txbody_get_txins

  // Accessor txouts: Ix ↦ TxOut
  protected def impl_txbody_get_txouts(txbody: R[TxBody]): R[Map[R[Ix], R[TxOut]]] =
    // TODO implement
    map_new()
  end impl_txbody_get_txouts

  lazy val txbody_get_txouts = f["txbody_get_txouts"] := p["txbody"] --> impl_txbody_get_txouts

  // Computation of size
  protected def impl_tx_size(tx: R[Tx]): R[Nat] =
    Todo("implement", 0)
  end impl_tx_size

  lazy val tx_size = f["tx_size"] := p["tx"] --> impl_tx_size

  // Note (error in spec?): Fig.10, p.16 defines a function
  //        txttl: Tx => Slot
  //       but the UTxO transition in fig.16, p.25 uses a predicate
  //        txttl txb >= slot
  //       where clearly the signature of the txttl function is
  //        txttl: TxBody => Slot.
  protected def impl_txttl(tx: R[Tx]): R[Slot] =
    val txbody = v["txbody"] := tx.get(Tx.body)
    val ttl    = v["ttl"]    := txbody.get(TxBody.ttl)

    ttl
  end impl_txttl

  lazy val txttl = f["txttl"] := p["tx"] --> impl_txttl
  
  // (computed) Accessor: txins: Set[TxIn]
  protected def impl_tx_get_txins(tx: R[Tx]): R[Set[R[TxIn]]] =
    val txbody = Tx.body(tx) // tx(Tx.body) = tx.get(Tx.body)
    val txins  = txbody_get_txins(txbody)

    txins
  end impl_tx_get_txins

  lazy val tx_get_txins = f["tx_get_txins"] := p["tx"] --> impl_tx_get_txins

  // (computed) Accessor txouts: Ix ↦ TxOut
  protected def impl_tx_get_txouts(tx: R[Tx]): R[Map[R[Ix], R[TxOut]]] =
    val txbody = Tx.body(tx)
    val txouts = txbody_get_txouts(txbody)

    txouts
  end impl_tx_get_txouts

  lazy val tx_get_txouts = f["tx_get_txouts"] := p["tx"] --> impl_tx_get_txouts

  /////////////////////////////////////////////////////////
  // Rules
  /////////////////////////////////////////////////////////
  lazy val PredError_Expired = v["PredError_Expired"] := new_predicate_error("PredError_Expired")

  protected def impl_premise_new(pred: R[Predicate], error: R[PredicateError]): R[Premise] =
    pair_new(pred, error)
  end impl_premise_new

  lazy val premise_new = f["premise_new"] := (p["pred"], p["error"]) --> impl_premise_new

  /////////////////////////////////////////////////////////
  // Constants
  /////////////////////////////////////////////////////////

  // fig.8, p.14 (constants)
  protected def impl_Slots_Per_Epoch: R[Nat]

  lazy val Slots_Per_Epoch = v["Slots_Per_Epoch"] := impl_Slots_Per_Epoch

  protected def impl_Slots_Per_KES_Period: R[Nat]

  lazy val Slots_Per_KES_Period = v["Slots_Per_KES_Period"] := impl_Slots_Per_KES_Period

  protected def impl_Stability_Window: R[Duration]

  lazy val Stability_Window = v["Stability_Window"] := impl_Stability_Window

  protected def impl_Network_Id: R[Network]

  lazy val Network_Id = v["Network_Id"] := impl_Network_Id

  //////////////////////////////
  // FIG 9 IS COMPLETE
  // fig.9, p.14
  // Denotational semantics of: minimum fee
  protected def impl_min_fee(pparams: R[PParams], tx: R[Tx]): R[Coin] =
    val a = v["a"] := pparams.get(PParams.a)
    val b = v["b"] := pparams.get(PParams.b)

    val n_txsize = v["n_txsize"] := tx_size(tx)
    val txsize   = v["txsize"]   := zat_of_nat(n_txsize)

    val z_coins = v["z_coins"] := a * txsize + b
    val coins   = v["coins"]   := coin_of_z(z_coins)

    coins
  end impl_min_fee

  lazy val min_fee = f["min_fee"] := (p["pparams"], p["tx"]) --> impl_min_fee
  
  // 2nd definition using let()
  protected def impl_min_fee2(pparams: R[PParams], tx: R[Tx]): R[Coin] =
    let( v["a"] := pparams.get(PParams.a)      )( a =>
    let( v["b"] := pparams.get(PParams.b)      )( b =>
    let( v["n_txsize"] := tx_size(tx)          )( n_txsize =>
    let( v["txsize"]   := zat_of_nat(n_txsize) )( txsize => 
    let( v["z_coins"]  := a * txsize + b       )( z_coins =>
    let( v["coins"]    := coin_of_z(z_coins)   )( coins =>
      coins
    ))))))
  end impl_min_fee2

  lazy val min_fee2 = f["min_fee2"] := (p["pparams"], p["tx"]) --> impl_min_fee2

  // fig.9, p.14
  // Denotational semantics of: epoch of a slot
  protected def impl_epoch(slot: R[Slot]): R[Epoch] =
    val slot_n  = v["slot_n"]  := n_of_slot(slot)
    val epoch_n = v["epoch_n"] := slot_n / Slots_Per_Epoch
    val result  = v["result"]  := epoch_of_n(epoch_n)

    result
  end impl_epoch

  lazy val epoch = f["epoch"] := p["slot"] --> impl_epoch

  // fig.9, p.14
  // Denotational semantics of: 1st slot of an epoch
  protected def impl_first_slot(e: R[Epoch]): R[Slot] =
    val e_n    = v["e_n"]    := n_of_epoch(e)
    val slot_n = v["slot_n"] := e_n * Slots_Per_Epoch
    val slot   = v["slot"]   := slot_of_n(slot_n)

    slot
  end impl_first_slot

  lazy val first_slot = f["first_slot"] := p["e"] --> impl_first_slot

  // fig.9, p.14 ledger-spec
  // Denotational semantics of: KES period of a slot
  protected def impl_kes_period(slot: R[Slot]): R[KESPeriod] =
    val n_slot = v["n_slot"] := n_of_slot(slot)
    val n_kesp = v["n_kesp"] := n_slot / Slots_Per_KES_Period
    val kesp   = v["kesp"]   := kes_period_of_n(n_kesp)

    kesp
  end impl_kes_period

  lazy val kes_period = f["kes_period"] := p["slot"] --> impl_kes_period

  // fig.12, p. 19
  // helper functions
  protected def impl_pv_can_follow(a: R[ProtVer], b: R[ProtVer]): R[Bool] =
    // (m, n)
    val m  = v["m"] := pair_first(a)
    val n  = v["n"] := pair_second(a)

    // (m_, n_), which is (m', n') in the PDF
    val m_ = v["m_"] := pair_first(b)
    val n_ = v["n_"] := pair_second(b)

    // (m + 1,  0) == (m', n')
    ((m + 1) === m_) && (0 === n_) ||
    // (m,  n + 1) == (m', n')
    (m === m_) && ((n + 1) === n_)
  end impl_pv_can_follow

  lazy val pv_can_follow = f["pv_can_follow"] := (p["a"], p["b"]) --> impl_pv_can_follow

  // Chain-specific function and variable definitions
  def Chain_Binds : List[Any] /* such an unfortunate failure of type inference :( */ = List(
    Slots_Per_Epoch,
    Slots_Per_KES_Period,
    Stability_Window,
    Network_Id,

    N_0, N_1, Z_0, Z_1,
    kes_period_of_n,
    epoch_of_n,
    n_of_epoch,
    coin_of_z, coin_of_n, coin_zero, coin_add,
    slot_gte,
    duration_of_slots,
    Networkid_mainnet, Networkid_testnet,
    Signal_New_Tx,
    utxo_new_empty,
    txbody_new, txbody_get_ttl, txbody_get_txins, txbody_get_txouts,
    tx_size,
    txttl,
    tx_get_txins, tx_get_txouts,
    PredError_Expired,
    premise_new,
    min_fee,
    epoch,
    first_slot,
    kes_period,
    pv_can_follow,
  )
  
  def Chain_Structs: StructDefs = List(
    KESPeriod.Struct,
    Epoch.Struct,
    Coin.Struct,
    Slot.Struct,
    Duration.Struct,
    Addr.Struct,
    PoolMD.Struct,
    TxIn.Struct,
    TxOut.Struct,
    TxBody.Struct,
    TxWitness.Struct,
    Tx.Struct,
    PPUpdateState.Struct,
    PPUpdateEnv.Struct,
    UTxOState.Struct,
    UTxOEnv.Struct,
    PParams.Struct,
  )
end Chain
