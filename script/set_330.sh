echo r > jlink/gd32f330/FlashMCU.jlink
echo h >> jlink/gd32f330/FlashMCU.jlink
echo loadbin $1 0x8000000 >> jlink/gd32f330/FlashMCU.jlink
echo g >> jlink/gd32f330/FlashMCU.jlink
echo verifybin $1 0x8000000 >> jlink/gd32f330/FlashMCU.jlink
echo r >> jlink/gd32f330/FlashMCU.jlink
echo q >> jlink/gd32f330/FlashMCU.jlink

