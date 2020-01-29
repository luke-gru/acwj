#include <stdio.h>

int mycall(int a, int b, int c, int d, int e) {
  return (1);
}

int main(void) {
  long i;
  i = 10;
  printf("%ld\n", i);
  /**
   * mycall() shouldn't clobber argument register 4 for printf() (rcx)
   * In order to do this, for a call to a function we make a first-pass thru
   * the argument list (forwards), and associate
   * them with an argument position. Those values are pushed on the stack.
   * Then when walking backwards to generate the cgloadargs, we pop the value
   * off the stack if it's a function call
   *
   * Try this first:
   * Brute force: for every function call cgloadarg, we push all argument
   * values to the stack before the call and pop them off after the call.
   */
  printf("%ld, %d, %ld\n", i, mycall(1, 2, 3, 4, 5), i);
  return (0);
}
