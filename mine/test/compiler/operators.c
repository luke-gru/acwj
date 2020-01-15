int main() {
  int a, b, x;
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

  return(0);
}
