### 一种简单的类似C语言的编译器

利用Koopa IR和LALRPOP实现的类似C语言的编译器，完成了[北大《编译原理实践》](https://pku-minic.github.io/online-doc/#/)的基本课程要求，作为Rust和编译器初学者的练习。非常感谢该课程助教提供的细致入微的文档。

编译器可以编译并运行类似如下的程序：

```C
// quicksort.c
int n;
int QuickSort(int arr[], int low, int high)
{
  if (low < high)
  {
    int i;
    i = low;
    int j;
    j = high;
    int k;
    k = arr[low];
    while (i < j)
    {
      while (i < j && arr[j] > k - 1)
      {
        j = j - 1;
      }

      if (i < j)
      {
        arr[i] = arr[j];
        i = i + 1;
      }

      while (i < j && arr[i] < k)
      {
        i = i + 1;
      }

      if (i < j)
      {
        arr[j] = arr[i];
        j = j - 1;
      }
    }

    arr[i] = k;
    int tmp;
    tmp = i - 1;
    tmp = QuickSort(arr, low, tmp);
    tmp = i + 1;
    tmp = QuickSort(arr, tmp, high);
  }
  return 0;
}

int main()
{
  n = 10;
  int a[10];
  a[0] = 4;
  a[1] = 3;
  a[2] = 9;
  a[3] = 2;
  a[4] = 0;
  a[5] = 1;
  a[6] = 6;
  a[7] = 5;
  a[8] = 7;
  a[9] = 8;
  int i;
  i = 0;
  int tmp;
  tmp = 9;
  i = QuickSort(a, i, tmp);
  while (i < n)
  {
    int tmp;
    tmp = a[i];
    putint(tmp);
    tmp = 10;
    putch(tmp);
    i = i + 1;
  }
  return 0;
}
```

运行方式：

```bash
# 生成抽象语法树
cargo run -- -ast quicksort.c
# 生成KoopaIR
cargo run -- -koopa quicksort.c -o quicksort.koopa
# 生成RISC-V汇编文件
cargo run -- -riscv quicksort.c -o quicksort.riscv
```