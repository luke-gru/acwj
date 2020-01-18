#include <stdio.h>
struct foo {
  int x;
  int y;
} fred, mary;

int main() {
  int x, y;
  x = 3; y = 2;
  fred.y = 20;
  fred.x = 10;
  printint(x+y+fred.x+fred.y);
  return(0);
}
