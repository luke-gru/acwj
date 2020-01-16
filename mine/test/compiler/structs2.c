int printf(char *fmt);

struct fred {                   // Struct declaration, done last time
  int x;
  char y;
  long z;
};

struct fred var2;               // Variable declaration, done last time
struct fred *varptr;            // Pointer variable declaration, done last time

int main() {
  long result;

  var2.x= 12;   printf("%d\n", var2.x);         // Member access as lvalue, new
  var2.y= 'c';  printf("%d\n", var2.y);
  var2.z= 4005; printf("%d\n", var2.z);

  result= var2.x + var2.y + var2.z;             // Member access as rvalue, new
  printf("%d\n", result);

  varptr= &var2;                                // Old behaviour
  result= varptr->x + varptr->y + varptr->z;    // Member access through pointer, new
  printf("%d\n", result);
  return(0);
}
