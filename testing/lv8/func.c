int var;

int f() {
  var = var + 1;
  return 2;
}

int main() {
  f();
  return var;
}