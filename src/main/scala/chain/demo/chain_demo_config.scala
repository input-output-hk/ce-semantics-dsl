package chain
package demo

import core.*

val DemoChainConfig = ChainConfig(
  100,
  3600,
  100,
  "testnet"
)

// A way to configure a Chain for demos.
trait ChainWithDemoConfig[I[_]] extends Chain[I]:
  // Const
  def impl_Slots_Per_Epoch: I[Nat] = nat(DemoChainConfig.Slots_Per_Epoch)
  def impl_Slots_Per_KES_Period: I[Nat] = nat(DemoChainConfig.Slots_Per_KES_Period)
  def impl_Stability_Window: I[Duration] = duration_of_nat(nat(DemoChainConfig.Stability_Window))
  def impl_Network_Id: I[Network] = str(DemoChainConfig.Network_Id)
end ChainWithDemoConfig
