#include <cstdlib>

#include <verilated.h>

#include "VtopEntity.h"

const int TIME = 50;

const int imem[256] =
 {133,145,221,180,230,237,230,237,193,60,251,107,0};
/* setl r0 5
 * setl r1 1
 * comp r1
 * setl r3 4
 * nand r2 r2
 * comp r2
 * nand r2 r2
 * comp r2
 * add  r0 r1
 * jo   r3
 * xor  r3 r3
 * out  r2 r3
 */
// Output: [0]: 12

int mem[256] = {0};

int main(int argc, char **argv) {
  Verilated::commandArgs(argc, argv);

  VtopEntity *top = new VtopEntity;
  /* top->
   *   Input : CLK EN RST Instr DataI
   *   Output: IAddr DAddr DataO SelRW SelMP SelEn
   */
  
  top->CLK = 0;
  top->EN = !0;
  top->RST = 0;
  top->Instr = imem[0];
  top->DataI = 0;
  
  int cnt = 0;
  while(!Verilated::gotFinish() && cnt < TIME) {
    if (!top->CLK) {  
      VL_PRINTF("cycle %d: %d %d %d\n", cnt, top->IAddr, top->DAddr, top->DataO);
      top->Instr = imem[top->IAddr];
      top->DataI = 0;
      if (top->SelEn) {
        if (top->SelMP) {
          if (top->SelRW) {
            VL_PRINTF("[%d]: %d\n", top->DAddr, top->DataO);
          } else {
            // Ignore Peripheral input, for the time being.
            top->DataI = 0;
          }
        } else {
          if (top->SelRW) {
            mem[top->DAddr] = top->DataO;
          } else {
            top->DataI = mem[top->DAddr];
          }
        }
      }
      cnt++;
    }
    
    top->CLK = !top->CLK;
    top->eval();
  }

  top->final();

  delete top;

  return EXIT_SUCCESS;
}
