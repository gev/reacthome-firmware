  
#ifndef _RUN_APP_BY_ADDR_H_
#define _RUN_APP_BY_ADDR_H_

static inline void run_app_by_addr(uint32_t addr){
  typedef void (*app_entry_t)(void);
  app_entry_t app_entry = (app_entry_t)(*(volatile uint32_t *) addr);
  app_entry();
}

#endif