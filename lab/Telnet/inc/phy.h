#ifndef _PHY_H_
#define _PHY_H_

#define PHY_ADDRESS                      ((uint16_t)0x1U)                         /*!< phy address determined by the hardware */

/* PHY read write timeouts */ 
#define PHY_READ_TO                      ((uint32_t)0x0004FFFFU)                /*!< PHY read timeout */
#define PHY_WRITE_TO                     ((uint32_t)0x0004FFFFU)                /*!< PHY write timeout */

/* PHY delay */
#define PHY_RESETDELAY                   ((uint32_t)0x008FFFFFU)                /*!< PHY reset delay */
#define PHY_CONFIGDELAY                  ((uint32_t)0x00FFFFFFU)                /*!< PHY configure delay */

/* PHY register address */ 
#define PHY_REG_BCR                      0U                                     /*!< tranceiver basic control register */
#define PHY_REG_BSR                      1U                                     /*!< tranceiver basic status register */

/* PHY basic control register */
#define PHY_RESET                        ((uint16_t)0x8000)                     /*!< PHY reset */
#define PHY_LOOPBACK                     ((uint16_t)0x4000)                     /*!< enable phy loop-back mode */
#define PHY_FULLDUPLEX_100M              ((uint16_t)0x2100)                     /*!< configure speed to 100 Mbit/s and the full-duplex mode */
#define PHY_HALFDUPLEX_100M              ((uint16_t)0x2000)                     /*!< configure speed to 100 Mbit/s and the half-duplex mode */
#define PHY_FULLDUPLEX_10M               ((uint16_t)0x0100)                     /*!< configure speed to 10 Mbit/s and the full-duplex mode */
#define PHY_HALFDUPLEX_10M               ((uint16_t)0x0000)                     /*!< configure speed to 10 Mbit/s and the half-duplex mode */
#define PHY_AUTONEGOTIATION              ((uint16_t)0x1000)                     /*!< enable auto-negotiation function */
#define PHY_RESTART_AUTONEGOTIATION      ((uint16_t)0x0200)                     /*!< restart auto-negotiation function */
#define PHY_POWERDOWN                    ((uint16_t)0x0800)                     /*!< enable the power down mode */
#define PHY_ISOLATE                      ((uint16_t)0x0400)                     /*!< isolate PHY from MII */

/* PHY basic status register */
#define PHY_AUTONEGO_COMPLETE            ((uint16_t)0x0020)                     /*!< auto-negotioation process completed */
#define PHY_LINKED_STATUS                ((uint16_t)0x0004)                     /*!< valid link established */
#define PHY_JABBER_DETECTION             ((uint16_t)0x0002)                     /*!< jabber condition detected */


#define PHY_SR                           0x1EU                                    /*!< tranceiver status register */
#define PHY_SPEED_STATUS                 ((uint16_t)0x0001)                     /*!< configured information of speed: 10Mbit/s */
#define PHY_DUPLEX_STATUS                ((uint16_t)0x0003)                     /*!< configured information of duplex: full-duplex */
// #elif(PHY_TYPE == DP83848)
// #define PHY_SR                           16U                                    /*!< tranceiver status register */
// #define PHY_SPEED_STATUS                 ((uint16_t)0x0002)                     /*!< configured information of speed: 10Mbit/s */
// #define PHY_DUPLEX_STATUS                ((uint16_t)0x0004)                     /*!< configured information of duplex: full-duplex */


#endif