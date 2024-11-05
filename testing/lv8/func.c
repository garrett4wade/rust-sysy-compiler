int half(int x)
{
  return x / 2;
}
const int c = 1, d = 10, a = 2;
int e;

void f(int a) { a = a + 1; }

int main()
{
  int a = 3;
  while (a < 10)
  {
    f(a);
  }
  putch(a);
  return half(a);
}