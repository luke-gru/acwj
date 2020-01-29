int main() {
  int i; int j;
  i=6; j=12;
  if (i < j) {
    printint(i); // => 6
  } else {
    printint(j);
  }

  if (i == 6 && j == 13) {
    printint(0);
  } else {
    printint(1); // => 1
  }

  if (i == 6 || j == 13) {
    printint(1); // => 1
  } else {
    printint(0);
  }

  if (j == 13 || i == 6) {
    printint(1); // => 1
  } else {
    printint(0);
  }

  if (i == 7 && j == 12) {
    printint(0);
  } else {
    printint(1); // => 1
  }
  return(0);
}
