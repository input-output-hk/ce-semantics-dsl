package chain

case class ChainConfig(
  Slots_Per_Epoch: Int,
  Slots_Per_KES_Period: Int,
  Stability_Window: Int,
  Network_Id: String
)

// A way to configure a Chain.
abstract class ChainWithConfigParam[I[_]](config: ChainConfig) extends Chain[I]:
  // Const
  def impl_Slots_Per_Epoch = nat(config.Slots_Per_Epoch)
  def impl_Slots_Per_KES_Period = nat(config.Slots_Per_KES_Period)
  def impl_Stability_Window = duration_of_nat(nat(config.Stability_Window))
  def impl_Network_Id = str(config.Network_Id)
end ChainWithConfigParam
