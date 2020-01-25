int main() {
  int a;
  int b;
  int x;
  a = b = 10;
  x = a + b;
  if (x) { printstring("x is not zero\n"); }
  if (!x) { printstring("x is zero\n"); }
  x = 0;
  if (x) { printstring("x is not zero\n"); }

  a = 1;
  printint(a++);
  printint(a);
  printint(++a);
  printint(a);
  printint(10%3);

  return(0);
}
