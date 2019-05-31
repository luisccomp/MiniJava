public class Exemplo {
  public static int fatorial(int n) {
    int fat, i;

    fat = 1;

    for (i = 1; i <= n; i ++) {
      fat *= i;
    }

    return fat;
  }
}
