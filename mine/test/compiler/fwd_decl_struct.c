struct intlist;

struct intlist {
  struct intlist *next;
  int val;
};

struct intlist first;
struct intlist last;

int main() {
  first.next = &last;
  first.val = 1;
  last.val = 2;
  printint(first.val);
  printint(last.val);
  return(0);
}
