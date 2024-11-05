int main() {
  int a = 1, b = 2;
  while (a < 10) {
    a = a + 1;
    while (a < 5 && b < 10) {
      b = b + 1;
    }
  }
  return a + b;
}