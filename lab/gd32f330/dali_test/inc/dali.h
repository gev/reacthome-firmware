#ifndef _DALI_H_
#define _DALI_H_

typedef struct {
  bool isEmpty;
  uint8_t byte;
} dali_recieve;

void dali_init();
dali_recieve read_byte();
void dali_send_frame(uint8_t, uint8_t);
void dali_send_special_command(uint8_t, uint8_t);
uint8_t find_ballasts(uint32_t*);


#define DALI_OFF 0 //0  - Turns off lighting.
#define DALI_UP 1 //1  - Increases the lighting control level for 200 ms according to the Fade rate.
#define DALI_DOWN 2 //2  - Decreases the lighting control level for 200 ms according to the Fade rate.
#define DALI_STEP_UP 3 //3  - Increments the lighting control level (without fade).
#define DALI_STEP_DOWN 4 //4  - Decrements the lighting control level (without fade).
#define DALI_RECALL_MAX_LEVEL 5 //5  - Maximizes the lighting control level (without fade).
#define DALI_RECALL_MIN_LEVEL 6 //6  - Minimizes the lighting control level (without fade)
#define DALI_STEP_DOWN_AND_OFF 7 //7  - Decrements the lighting control level and turns off lighting if the level is at the minimum (without fade).
#define DALI_ON_AND_STEP_UP 8 //8  - Increments the lighting control level and turns on lighting if lighting is off (with fade). 
#define DALI_ENABLE_DAPC_SEQUENCE 9 //9  - It shows the repeat start of the DAPC command.
#define DALI_GO_TO_LAST_ACTIVE_LEVEL 10 //10 DALI-2 - Adjusts the lighting control level to the last light control level according to the Fade time. (Command that exist only in IEC62386-102ed2.0)
#define DALI_GO_TO_SCENE0 16 //16  - Adjusts the lighting control level for Scene XXXX according to the fade time.
#define DALI_GO_TO_SCENE1 17 //17  - Adjusts the lighting control level for Scene XXXX according to the fade time.
#define DALI_GO_TO_SCENE2 18 //18  - Adjusts the lighting control level for Scene XXXX according to the fade time.
#define DALI_GO_TO_SCENE3 19 //19  - Adjusts the lighting control level for Scene XXXX according to the fade time.
#define DALI_GO_TO_SCENE4 20 //20  - Adjusts the lighting control level for Scene XXXX according to the fade time.
#define DALI_GO_TO_SCENE5 21 //21  - Adjusts the lighting control level for Scene XXXX according to the fade time.
#define DALI_GO_TO_SCENE6 22 //22  - Adjusts the lighting control level for Scene XXXX according to the fade time.
#define DALI_GO_TO_SCENE7 23 //23  - Adjusts the lighting control level for Scene XXXX according to the fade time.
#define DALI_GO_TO_SCENE8 24 //24  - Adjusts the lighting control level for Scene XXXX according to the fade time.
#define DALI_GO_TO_SCENE9 25 //25  - Adjusts the lighting control level for Scene XXXX according to the fade time.
#define DALI_GO_TO_SCENE10 26 //26  - Adjusts the lighting control level for Scene XXXX according to the fade time.
#define DALI_GO_TO_SCENE11 27 //27  - Adjusts the lighting control level for Scene XXXX according to the fade time.
#define DALI_GO_TO_SCENE12 28 //28  - Adjusts the lighting control level for Scene XXXX according to the fade time.
#define DALI_GO_TO_SCENE13 29 //29  - Adjusts the lighting control level for Scene XXXX according to the fade time.
#define DALI_GO_TO_SCENE14 30 //30  - Adjusts the lighting control level for Scene XXXX according to the fade time.
#define DALI_GO_TO_SCENE15 31 //31  - Adjusts the lighting control level for Scene XXXX according to the fade time.
#define DALI_RESET 544 //32 REPEAT - Makes a slave an RESET state.
#define DALI_STORE_ACTUAL_LEVEL_IN_THE_DTR0 545 //33 REPEAT - Saves the current lighting control level to the DTR (DTR0). (In the parenthesis is a name in IEC62386-102ed2.0)
#define DALI_SAVE_PERSISTENT_VARIABLES 546 //34 REPEAT DALI-2 - Saves a variable in nonvolatile memory (NVM). (Command that exist only in IEC62386-102ed2.0)
#define DALI_SET_OPERATING_MODE 547 //35 REPEAT DALI-2 - Sets data of DTR0 as an operating mode. (Command that exist only in IEC62386-102ed2.0)
#define DALI_RESET_MEMORY_BANK 548 //36 REPEAT DALI-2 - Changes to the reset value the specified memory bank in DTR0. (Command that exist only in IEC62386-102ed2.0)
#define DALI_IDENTIFY_DEVICE 549 //37 REPEAT DALI-2 - Starts the identification state of the device. (Command that exist only in IEC62386-102ed2.0)
#define DALI_RESERVED38 550 //38 REPEAT - [Reserved]
#define DALI_RESERVED39 551 //39 REPEAT - [Reserved]
#define DALI_RESERVED40 552 //40 REPEAT - [Reserved]
#define DALI_RESERVED41 553 //41 REPEAT - [Reserved]
#define DALI_SET_MAX_LEVEL 554 //42 REPEAT - Specifies the DTR data as the maximum lighting control level. (In the parenthesis is a name in IEC62386-102ed2.0)
#define DALI_SET_MIN_LEVEL 555 //43 REPEAT - Specifies the DTR data as the minimum lighting control level. (In the parenthesis is a name in IEC62386-102ed2.0)
#define DALI_SET_SYSTEM_FAILURE_LEVEL 556 //44 REPEAT - Specifies the DTR data as the "FAILURELEVEL". (In the parenthesis is a name in IEC62386-102ed2.0)
#define DALI_SET_POWER_ON_LEVEL 557 //45 REPEAT - Specifies the DTR data as the "POWER ONLEVEL". (In the parenthesis is a name in IEC62386-102ed2.0)
#define DALI_SET_FADE_TIME 558 //46 REPEAT - Specifies the DTR data as the Fade time. (In the parenthesis is a name in IEC62386-102ed2.0)
#define DALI_SET_FADE_RATE 559 //47 REPEAT - Specifies the DTR data as the Fade rate. (In the parenthesis is a name in IEC62386-102ed2.0)
#define DALI_SET_EXTENDED_FADE_TIME 560 //48 REPEAT DALI-2 - Specifies the DTR data as the Extended Fade Time. (Command that exist only in IEC62386-102ed2.0)
#define DALI_SET_SCENE0 576 //64 REPEAT - Specifies the DTR data as Scene XXXX. (In the parenthesis is a name in IEC62386-102ed2.0)
#define DALI_SET_SCENE1 577 //65 REPEAT - Specifies the DTR data as Scene XXXX. (In the parenthesis is a name in IEC62386-102ed2.0)
#define DALI_SET_SCENE2 578 //66 REPEAT - Specifies the DTR data as Scene XXXX. (In the parenthesis is a name in IEC62386-102ed2.0)
#define DALI_SET_SCENE3 579 //67 REPEAT - Specifies the DTR data as Scene XXXX. (In the parenthesis is a name in IEC62386-102ed2.0)
#define DALI_SET_SCENE4 580 //68 REPEAT - Specifies the DTR data as Scene XXXX. (In the parenthesis is a name in IEC62386-102ed2.0)
#define DALI_SET_SCENE5 581 //69 REPEAT - Specifies the DTR data as Scene XXXX. (In the parenthesis is a name in IEC62386-102ed2.0)
#define DALI_SET_SCENE6 582 //70 REPEAT - Specifies the DTR data as Scene XXXX. (In the parenthesis is a name in IEC62386-102ed2.0)
#define DALI_SET_SCENE7 583 //71 REPEAT - Specifies the DTR data as Scene XXXX. (In the parenthesis is a name in IEC62386-102ed2.0)
#define DALI_SET_SCENE8 584 //72 REPEAT - Specifies the DTR data as Scene XXXX. (In the parenthesis is a name in IEC62386-102ed2.0)
#define DALI_SET_SCENE9 585 //73 REPEAT - Specifies the DTR data as Scene XXXX. (In the parenthesis is a name in IEC62386-102ed2.0)
#define DALI_SET_SCENE10 586 //74 REPEAT - Specifies the DTR data as Scene XXXX. (In the parenthesis is a name in IEC62386-102ed2.0)
#define DALI_SET_SCENE11 587 //75 REPEAT - Specifies the DTR data as Scene XXXX. (In the parenthesis is a name in IEC62386-102ed2.0)
#define DALI_SET_SCENE12 588 //76 REPEAT - Specifies the DTR data as Scene XXXX. (In the parenthesis is a name in IEC62386-102ed2.0)
#define DALI_SET_SCENE13 589 //77 REPEAT - Specifies the DTR data as Scene XXXX. (In the parenthesis is a name in IEC62386-102ed2.0)
#define DALI_SET_SCENE14 590 //78 REPEAT - Specifies the DTR data as Scene XXXX. (In the parenthesis is a name in IEC62386-102ed2.0)
#define DALI_SET_SCENE15 591 //79 REPEAT - Specifies the DTR data as Scene XXXX. (In the parenthesis is a name in IEC62386-102ed2.0)
#define DALI_REMOVE_FROM_SCENE0 592 //80 REPEAT - Deletes the Scene XXXX setting. (Specifies 1111 1111 for the scene register.)
#define DALI_REMOVE_FROM_SCENE1 593 //81 REPEAT - Deletes the Scene XXXX setting. (Specifies 1111 1111 for the scene register.)
#define DALI_REMOVE_FROM_SCENE2 594 //82 REPEAT - Deletes the Scene XXXX setting. (Specifies 1111 1111 for the scene register.)
#define DALI_REMOVE_FROM_SCENE3 595 //83 REPEAT - Deletes the Scene XXXX setting. (Specifies 1111 1111 for the scene register.)
#define DALI_REMOVE_FROM_SCENE4 596 //84 REPEAT - Deletes the Scene XXXX setting. (Specifies 1111 1111 for the scene register.)
#define DALI_REMOVE_FROM_SCENE5 597 //85 REPEAT - Deletes the Scene XXXX setting. (Specifies 1111 1111 for the scene register.)
#define DALI_REMOVE_FROM_SCENE6 598 //86 REPEAT - Deletes the Scene XXXX setting. (Specifies 1111 1111 for the scene register.)
#define DALI_REMOVE_FROM_SCENE7 599 //87 REPEAT - Deletes the Scene XXXX setting. (Specifies 1111 1111 for the scene register.)
#define DALI_REMOVE_FROM_SCENE8 600 //88 REPEAT - Deletes the Scene XXXX setting. (Specifies 1111 1111 for the scene register.)
#define DALI_REMOVE_FROM_SCENE9 601 //89 REPEAT - Deletes the Scene XXXX setting. (Specifies 1111 1111 for the scene register.)
#define DALI_REMOVE_FROM_SCENE10 602 //90 REPEAT - Deletes the Scene XXXX setting. (Specifies 1111 1111 for the scene register.)
#define DALI_REMOVE_FROM_SCENE11 603 //91 REPEAT - Deletes the Scene XXXX setting. (Specifies 1111 1111 for the scene register.)
#define DALI_REMOVE_FROM_SCENE12 604 //92 REPEAT - Deletes the Scene XXXX setting. (Specifies 1111 1111 for the scene register.)
#define DALI_REMOVE_FROM_SCENE13 605 //93 REPEAT - Deletes the Scene XXXX setting. (Specifies 1111 1111 for the scene register.)
#define DALI_REMOVE_FROM_SCENE14 606 //94 REPEAT - Deletes the Scene XXXX setting. (Specifies 1111 1111 for the scene register.)
#define DALI_REMOVE_FROM_SCENE15 607 //95 REPEAT - Deletes the Scene XXXX setting. (Specifies 1111 1111 for the scene register.)
#define DALI_ADD_TO_GROUP0 608 //96 REPEAT - Adds the slave to Group XXXX.
#define DALI_ADD_TO_GROUP1 609 //97 REPEAT - Adds the slave to Group XXXX.
#define DALI_ADD_TO_GROUP2 610 //98 REPEAT - Adds the slave to Group XXXX.
#define DALI_ADD_TO_GROUP3 611 //99 REPEAT - Adds the slave to Group XXXX.
#define DALI_ADD_TO_GROUP4 612 //100 REPEAT - Adds the slave to Group XXXX.
#define DALI_ADD_TO_GROUP5 613 //101 REPEAT - Adds the slave to Group XXXX.
#define DALI_ADD_TO_GROUP6 614 //102 REPEAT - Adds the slave to Group XXXX.
#define DALI_ADD_TO_GROUP7 615 //103 REPEAT - Adds the slave to Group XXXX.
#define DALI_ADD_TO_GROUP8 616 //104 REPEAT - Adds the slave to Group XXXX.
#define DALI_ADD_TO_GROUP9 617 //105 REPEAT - Adds the slave to Group XXXX.
#define DALI_ADD_TO_GROUP10 618 //106 REPEAT - Adds the slave to Group XXXX.
#define DALI_ADD_TO_GROUP11 619 //107 REPEAT - Adds the slave to Group XXXX.
#define DALI_ADD_TO_GROUP12 620 //108 REPEAT - Adds the slave to Group XXXX.
#define DALI_ADD_TO_GROUP13 621 //109 REPEAT - Adds the slave to Group XXXX.
#define DALI_ADD_TO_GROUP14 622 //110 REPEAT - Adds the slave to Group XXXX.
#define DALI_ADD_TO_GROUP15 623 //111 REPEAT - Adds the slave to Group XXXX.
#define DALI_REMOVE_FROM_GROUP0 624 //112 REPEAT - Deletes the slave from Group XXXX.
#define DALI_REMOVE_FROM_GROUP1 625 //113 REPEAT - Deletes the slave from Group XXXX.
#define DALI_REMOVE_FROM_GROUP2 626 //114 REPEAT - Deletes the slave from Group XXXX.
#define DALI_REMOVE_FROM_GROUP3 627 //115 REPEAT - Deletes the slave from Group XXXX.
#define DALI_REMOVE_FROM_GROUP4 628 //116 REPEAT - Deletes the slave from Group XXXX.
#define DALI_REMOVE_FROM_GROUP5 629 //117 REPEAT - Deletes the slave from Group XXXX.
#define DALI_REMOVE_FROM_GROUP6 630 //118 REPEAT - Deletes the slave from Group XXXX.
#define DALI_REMOVE_FROM_GROUP7 631 //119 REPEAT - Deletes the slave from Group XXXX.
#define DALI_REMOVE_FROM_GROUP8 632 //120 REPEAT - Deletes the slave from Group XXXX.
#define DALI_REMOVE_FROM_GROUP9 633 //121 REPEAT - Deletes the slave from Group XXXX.
#define DALI_REMOVE_FROM_GROUP10 634 //122 REPEAT - Deletes the slave from Group XXXX.
#define DALI_REMOVE_FROM_GROUP11 635 //123 REPEAT - Deletes the slave from Group XXXX.
#define DALI_REMOVE_FROM_GROUP12 636 //124 REPEAT - Deletes the slave from Group XXXX.
#define DALI_REMOVE_FROM_GROUP13 637 //125 REPEAT - Deletes the slave from Group XXXX.
#define DALI_REMOVE_FROM_GROUP14 638 //126 REPEAT - Deletes the slave from Group XXXX.
#define DALI_REMOVE_FROM_GROUP15 639 //127 REPEAT - Deletes the slave from Group XXXX.
#define DALI_SET_SHORT_ADDRESS 640 //128 REPEAT - Specifies the DTR data as a Short Address. (In the parenthesis is a name in IEC62386-102ed2.0)
#define DALI_ENABLE_WRITE_MEMORY 641 //129 REPEAT - Allows writing of the memory bank.
#define DALI_RESERVED130 642 //130 REPEAT - [Reserved]
#define DALI_RESERVED131 643 //131 REPEAT - [Reserved]
#define DALI_RESERVED132 644 //132 REPEAT - [Reserved]
#define DALI_RESERVED133 645 //133 REPEAT - [Reserved]
#define DALI_RESERVED134 646 //134 REPEAT - [Reserved]
#define DALI_RESERVED135 647 //135 REPEAT - [Reserved]
#define DALI_RESERVED136 648 //136 REPEAT - [Reserved]
#define DALI_RESERVED137 649 //137 REPEAT - [Reserved]
#define DALI_RESERVED138 650 //138 REPEAT - [Reserved]
#define DALI_RESERVED139 651 //139 REPEAT - [Reserved]
#define DALI_RESERVED140 652 //140 REPEAT - [Reserved]
#define DALI_RESERVED141 653 //141 REPEAT - [Reserved]
#define DALI_RESERVED142 654 //142 REPEAT - [Reserved]
#define DALI_RESERVED143 655 //143 REPEAT - [Reserved]
#define DALI_QUERY_STATUS 144 //144  - Returns "STATUS INFORMATION"
#define DALI_QUERY_CONTROL_GEAR_PRESENT 145 //145  - Is there a slave that can communicate? (In the parenthesis is a name in IEC62386-102ed2.0)
#define DALI_QUERY_LAMP_FAILURE 146 //146  - Is there a lamp problem?
#define DALI_QUERY_LAMP_POWER_ON 147 //147  - Is a lamp on?
#define DALI_QUERY_LIMIT_ERROR 148 //148  - Is the specified lighting control level out of the range from the minimum to the maximum values?
#define DALI_QUERY_RESET_STATE 149 //149  - Is the slave in 'RESET STATE'?
#define DALI_QUERY_MISSING_SHORT_ADDRESS 150 //150  - Does the slave not have a short address?
#define DALI_QUERY_VERSION_NUMBER 151 //151  - What is the corresponding IEC standard number?
#define DALI_QUERY_CONTENT_DTR0 152 //152  - What is the DTR content? (In the parenthesis is a name in IEC62386-102ed2.0)
#define DALI_QUERY_DEVICE_TYPE 153 //153  - What is the device type?  (fluorescent lamp:0000 0000) (IEC62386-207 is 6 fixed)
#define DALI_QUERY_PHYSICAL_MINIMUM_LEVEL 154 //154  - What is the minimum lighting control level specified by the hardware?
#define DALI_QUERY_POWER_FAILURE 155 //155  - Has the slave operated without the execution of reset-command or the adjustment of the lighting control level?
#define DALI_QUERY_CONTENT_DTR1 156 //156  - What is the DTR1 content?
#define DALI_QUERY_CONTENT_DTR2 157 //157  - What is the DTR2 content?
// #define DALI_QUERY_OPERATING_MODE 158 //158 DALI-2 - What is the Operating Mode? (Only IEC62386-102ed2.0 )
#define DALI_QUERY_LIGHT_SOURCE_TYPE 159 //159 DALI-2 - What is the Light source type? (Only IEC62386-102ed2.0 )
#define DALI_QUERY_ACTUAL_LEVEL 160 //160  - What is the "ACTUAL LEVEL" (the current lighting control level)?
#define DALI_QUERY_MAX_LEVEL 161 //161  - What is the maximum lighting control level?
#define DALI_QUERY_MIN_LEVEL 162 //162  - What is the minimum lighting control level?
#define DALI_QUERY_POWER_ON_LEVEL 163 //163  - What is the "POWER ON LEVEL" (the lighting control level when the power is turned on)?
#define DALI_QUERY_SYSTEM_FAILURE_LEVEL 164 //164  - What is the "SYSTEM FAILURE LEVEL" (the lighting control level when a failure occurs)?
#define DALI_QUERY_FADE_TIME_FADE_RATE 165 //165  - What are the Fade time and Fade rate?
#define DALI_QUERY_MANUFACTURER_SPECIFIC_MODE 166 //166 DALI-2 - What is the Specific Mode? (Command that exist only in IEC62386-102ed2.0)
#define DALI_QUERY_NEXT_DEVICE_TYPE 167 //167 DALI-2 - What is the next Device Type? (Command that exist only in IEC62386-102ed2.0)
#define DALI_QUERY_EXTENDED_FADE_TIME 168 //168 DALI-2 - What is the Extended Fade Time? (Command that exist only in IEC62386-102ed2.0)
#define DALI_QUERY_CONTROL_GEAR_FAILURE 169 //169 DALI-2 - Does a slave have the abnormality? (Command that exist only in IEC62386-102ed2.0)
#define DALI_QUERY_SCENE0_LEVEL 176 //176  - What is the lighting control level for SCENE XXXX?
#define DALI_QUERY_SCENE1_LEVEL 177 //177  - What is the lighting control level for SCENE XXXX?
#define DALI_QUERY_SCENE2_LEVEL 178 //178  - What is the lighting control level for SCENE XXXX?
#define DALI_QUERY_SCENE3_LEVEL 179 //179  - What is the lighting control level for SCENE XXXX?
#define DALI_QUERY_SCENE4_LEVEL 180 //180  - What is the lighting control level for SCENE XXXX?
#define DALI_QUERY_SCENE5_LEVEL 181 //181  - What is the lighting control level for SCENE XXXX?
#define DALI_QUERY_SCENE6_LEVEL 182 //182  - What is the lighting control level for SCENE XXXX?
#define DALI_QUERY_SCENE7_LEVEL 183 //183  - What is the lighting control level for SCENE XXXX?
#define DALI_QUERY_SCENE8_LEVEL 184 //184  - What is the lighting control level for SCENE XXXX?
#define DALI_QUERY_SCENE9_LEVEL 185 //185  - What is the lighting control level for SCENE XXXX?
#define DALI_QUERY_SCENE10_LEVEL 186 //186  - What is the lighting control level for SCENE XXXX?
#define DALI_QUERY_SCENE11_LEVEL 187 //187  - What is the lighting control level for SCENE XXXX?
#define DALI_QUERY_SCENE12_LEVEL 188 //188  - What is the lighting control level for SCENE XXXX?
#define DALI_QUERY_SCENE13_LEVEL 189 //189  - What is the lighting control level for SCENE XXXX?
#define DALI_QUERY_SCENE14_LEVEL 190 //190  - What is the lighting control level for SCENE XXXX?
#define DALI_QUERY_SCENE15_LEVEL 191 //191  - What is the lighting control level for SCENE XXXX?
#define DALI_QUERY_GROUPS_0_7 192 //192  - Does the slave belong to a group among groups 0 to 7? (Each bit corresponds to agroup.)
#define DALI_QUERY_GROUPS_8_15 193 //193  - Does the slave belong to a group among groups 8 to 15? (Each bit corresponds to agroup.)
#define DALI_QUERY_RANDOM_ADDRESS_H 194 //194  - What are the higher 8 bits of the random address?
#define DALI_QUERY_RANDOM_ADDRESS_M 195 //195  - What are the middle 8 bits of the random address?
#define DALI_QUERY_RANDOM_ADDRESS_L 196 //196  - What are the lower 8 bits of the random address?
#define DALI_READ_MEMORY_LOCATION 197 //197  - What is the memory location content? 
#define DALI_REFERENCE_SYSTEM_POWER 224 //224 IEC62386-207 - Starts power measurement. (Command that exist only in IEC62386-207)
#define DALI_ENABLE_CURRENT_PROTECTOR 225 //225 IEC62386-207 - Enables the current protection. (Command that exist only in IEC62386-207)
#define DALI_DISABLE_CURRENT_PROTECTOR 226 //226 IEC62386-207 - Disables the current protection. (Command that exist only in IEC62386-207)
#define DALI_SELECT_DIMMING_CURVE 227 //227 IEC62386-207 - Selects Dimming curve. (Command that exist only in IEC62386-207)
#define DALI_STORE_DTR_AS_FAST_FADE_TIME 228 //228 IEC62386-207 - Sets the DTR of the data as Fast Fade Time.(Command that exist only in IEC62386-207)
#define DALI_QUERY_GEAR_TYPE 237 //237 IEC62386-207 - Returns ‘GEAR TYPE’ (Command that exist only in IEC62386-207)
#define DALI_QUERY_DIMMING_CURVE 238 //238 IEC62386-207 - Returns ’Dimming curve’in use (Command that exist only in IEC62386-207)
#define DALI_QUERY_POSSIBLE_OPERATING_MODE 239 //239 IEC62386-207 - Returns ‘POSSIBLEG OPERATING MODE’ (Command that exist only in IEC62386-207)
#define DALI_QUERY_FEATURES 240 //240 IEC62386-207 - Returns ‘FEATURES’ (Command that exist only in IEC62386-207)
#define DALI_QUERY_FAILURE_STATUS 241 //241 IEC62386-207 - Returns ‘FAILURE STATUS’ (Command that exist only in IEC62386-207)
#define DALI_QUERY_SHORT_CIRCUIT 242 //242 IEC62386-207 - Returns bit0 short circuit of ‘FAILURE STATUS’ (Command that exist only in IEC62386-207)
#define DALI_QUERY_OPEN_CIRCUIT 243 //243 IEC62386-207 - Returns bit1 open circuit of ‘FAILURE STATUS’ (Command that exist only in IEC62386-207)
#define DALI_QUERY_LOAD_DECREASE 244 //244 IEC62386-207 - Returns bit2 load decrease of ‘FAILURE STATUS’ (Command that exist only in IEC62386-207)
#define DALI_QUERY_LOAD_INDREASE 245 //245 IEC62386-207 - Returns bit3 load increase of‘FAILURE STATUS’ (Command that exist only in IEC62386-207)
#define DALI_QUERY_CURRENT_PROTECTOR_ACTIVE 246 //246 IEC62386-207 - Returns bit4 current protector active of ‘FAILURE STATUS’ (Command that exist only in IEC62386-207)
#define DALI_QUERY_THERMAL_SHUTDOWN 247 //247 IEC62386-207 - Returns bit5 thermal shut down of ‘FAILURE STATUS’ (Command that exist only in IEC62386-207)
#define DALI_QUERY_THERMAL_OVERLOAD 248 //248 IEC62386-207 - Returns bit6 thermal overload with light level reduction of ‘FAILURE STATUS’ (Command that exist only in IEC62386-207)
#define DALI_QUERY_REFARENCE_RUNNING 249 //249 IEC62386-207 - Returns whetherReference System Power is in operation. (Command that exist only in IEC62386-207)
#define DALI_QUERY_REFERENCE_MEASURMENT_FAILED 250 //250 IEC62386-207 - Returns bit7 reference measurement failed  of ‘FAILURE STATUS’ (Command that exist only in IEC62386-207)
#define DALI_QUERY_CURRENT_PROTECTOR_ENABLE 251 //251 IEC62386-207 - Returns state of Curent protector (Command that exist only in IEC62386-207)
#define DALI_QUERY_OPERATING_MODE 252 //252 IEC62386-207 - Returns ‘OPERATING MODE’ (Command that exist only in IEC62386-207)
#define DALI_QUERY_FAST_FADE_TIME 253 //253 IEC62386-207 - Returns set Fast fade time. (Command that exist only in IEC62386-207)
#define DALI_QUERY_MIN_FAST_FADE_TIME 254 //254 IEC62386-207 - Returns set Minimum fast fade time (Command that exist only in IEC62386-207)
#define DALI_QUERY_EXTENDED_VERSION_NUMBER 255 //255 IEC62386-207 - The version number of the extended support? IEC62386-207: 1, Other: NO(no response)
// SPECIAL COMMANDS:
#define DALI_TERMINATE 0xA1 //256  - Releases the INITIALISE state.
#define DALI_DATA_TRANSFER_REGISTER0 0xA3 //257  - Stores the data XXXX XXXX to the DTR(DTR0). (In the parenthesis is a name in IEC62386-102ed2.0)
#define DALI_INITIALISE 0xA5 //258 REPEAT - Sets the slave to the INITIALISE status for15 minutes. Commands 259 to 270 are enabled only for a slave in this status.
#define DALI_RANDOMISE 0xA7 //259 REPEAT - Generates a random address.
#define DALI_COMPARE 0xA9 //260  - Is the random address smaller or equal to the search address?
#define DALI_WITHDRAW 0xAB //261  - Excludes slaves for which the random address and search address match from the Compare process.
#define DALI_RESERVED262 0xAD //262  - [Reserved]
#define DALI_PING 0xAF //263 DALI-2 - Ignores in the slave. (Command that exist only in IEC62386-102ed2.0)
#define DALI_SEARCHADDRH 0xB1 //264  - Specifies the higher 8 bits of the search address.
#define DALI_SEARCHADDRM 0xB3 //265  - Specifies the middle 8 bits of the search address.
#define DALI_SEARCHADDRL 0xB5 //266  - Specifies the lower 8 bits of the search address.
#define DALI_PROGRAM_SHORT_ADDRESS 0xB7 //267  - The slave shall store the received 6-bit address (AAA AAA) as a short address if it is selected.
#define DALI_VERIFY_SHORT_ADDRESS 0xB9 //268  - Is the short address AAA AAA?
#define DALI_QUERY_SHORT_ADDRESS 0xBB //269  - What is the short address of the slaveNote 2being selected?
#define DALI_PHYSICAL_SELECTION 0xBD //270 not DALI-2 - Sets the slave to Physical Selection Mode and excludes the slave from the Compare process. (Excluding IEC62386-102ed2.0) (Command that exist only in IEC62386-102ed1.0, -207ed1.0)
#define DALI_RESERVED271 0xBF //271  - [Reserved]
#define DALI_ENABLE_DEVICE_TYPE_X 0xC1 //272  - Adds the device XXXX (a special device).
#define DALI_DATA_TRANSFER_REGISTER1 0xC3 //273  - Stores data XXXX into DTR1.
#define DALI_DATA_TRANSFER_REGISTER2 0xC5 //274  - Stores data XXXX into DTR2.
#define DALI_WRITE_MEMORY_LOCATION 0xC7 //275  - Write data into the specified address of the specified memory bank. (There is BW) (DTR(DTR0)：address, DTR1：memory bank number)
#define DALI_WRITE_MEMORY_LOCATION_NO_REPLY 0xC9 //276 DALI-2 - Write data into the specified address of the specified memory bank. (There is no BW) (DTR(DTR0)：address, TR1：memory bank number) (Command that exist only in IEC62386-102ed2.0)


#endif