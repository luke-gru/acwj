int open(char *pathname, int flags);
int read(int fd, char *buf, int count);
int write(int fd, void *buf, int count);
int close(int fd);
void perror(char *err);
char *buf;

int main() {
  int zin;
  int cnt;

  buf= "                                                             ";

  zin = open("test/compiler/basic_cat.c", 0);
  if (zin == -1) {
    printstring("open 'test/compiler/basic_cat.c' failed\n");
    return (1);
  }
  while ((cnt = read(zin, buf, 60)) > 0) {
    write(1, buf, cnt);
  }
  return(0);
}
