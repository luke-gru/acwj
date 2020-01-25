char *filename;
int set_filename(void) {
  int a = 1;
  filename = "myfilename";
  switch (a) {
    case 0:
    case 1:
      a = 2;
      break;
    default:
      a = 3;
  }
  return (a);
}
