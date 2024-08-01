#include <cstdlib>

#include <verilated.h>

#include "VtestTop.h"

const int TIME = 50;

int main(int argc, char **argv) {
  Verilated::commandArgs(argc, argv);

  VtestTop *top = new VtestTop;
  
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
  
  int cnt = 0;
  while(!Verilated::gotFinish() && cnt < TIME) {
    if (!top->CLK) {
      VL_PRINTF("cycle %d: %d\n", cnt, top->OUT);
      cnt++;
    }
    
    top->CLK = !top->CLK;
    top->eval();
  }

  top->final();

  delete top;

  return EXIT_SUCCESS;
}
