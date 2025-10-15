FILE="jlink/gd32f330/FlashMCU.jlink"

echo r > $FILE
echo h >> $FILE
echo loadbin $1 0x8000000 >> $FILE
echo g >> $FILE
echo verifybin $1 0x8000000 >> $FILE
echo r >> $FILE
echo q >> $FILE