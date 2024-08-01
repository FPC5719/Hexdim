#include <cstdlib>

#include <nvboard.h>

#include "VnvTop.h"

int main() {
  VnvTop *top = new VnvTop;
  
  nvboard_bind_pin( &top->Seg0, 8, SEG0A, SEG0B, SEG0C, SEG0D, SEG0E, SEG0F, SEG0G, DEC0P);
  nvboard_bind_pin( &top->Seg1, 8, SEG1A, SEG1B, SEG1C, SEG1D, SEG1E, SEG1F, SEG1G, DEC1P);

  nvboard_init();

  top->CLK = 0;
  top->EN = !0;
  top->RST = !0;

  int rstcnt = 10;
  while (rstcnt > 0) {
    top->CLK = !top->CLK;
    top->eval();
    rstcnt--;
  }
  top->RST = 0;
  
  while(true) {
    nvboard_update();
    
    top->CLK = !top->CLK;
    top->eval();
  }

  top->final();

  delete top;

  nvboard_quit();

  return 0;
}
