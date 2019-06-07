import java.util.Scanner;

public class Micro10 {
    public static void main(String[] args) {
        int numero;
        int fat;
        Scanner s = new Scanner(System.in);

        System.out.printf("Digite um numero: ");
        numero = Integer.parseInt(s.nextLine());
        fat = fatorial(numero);
    
        System.out.printf("O fatorial de ");
        System.out.printf("%d", numero);
        System.out.printf(" e ");
        System.out.printf("%d", fat);
    }

    public static int fatorial(int n) {
        if (n <= 0) {
            return 1;
        }
        else {
            return n * fatorial(n - 1);
        }
    }
}

