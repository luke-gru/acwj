char *str;
int main() {
    for (str= "Hello world\n"; *str != 0; str= str + 1) {
        printchar(*str);
    }
    return(0);
}
