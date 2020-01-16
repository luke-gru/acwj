void print_ints(int ary[], int num) {
  int i;
  i = 0;
  while (i < num) {
    printint(ary[i]);
    i++;
  }
}

int main() {
  int ints[3];
  ints[0] = 10;
  ints[1] = 100;
  ints[2] = 1000;
  print_ints(ints, 3);
  return (0);
}
