extern int foo;
int main() {
  printint(foo); // NOTE: linker won't accept this, undefined symbol
  return(0);
}
