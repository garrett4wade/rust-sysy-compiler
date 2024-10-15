int main() {
  int x = 10;
  const int aa =10,bb=11+aa;
  x = x + 1;
  int y = x + aa;
  return x * bb;
}