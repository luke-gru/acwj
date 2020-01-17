enum fred { a, b, c };                  // a is 0, b is 1, c is 2
enum foo  { d=2, e=6, f };              // d is 2, e is 6, f is 7
enum bar  { g=2, h=6, i } var1;         // var1 is really an int
enum      { j, k, l }     var2;         // var2 is really an int

int main() {
  printint(var1); // 0
  printint(var2); // 0
  printstring("\n");

  printint(a);
  printint(b);
  printint(c);
  printstring("\n");

  printint(d);
  printint(e);
  printint(f);
  printstring("\n");

  var1 = g;
  printint(var1);
  printint(h);
  printint(i);
  printstring("\n");

  printint(j);
  printint(k);
  printint(l);
  return(0);
}
