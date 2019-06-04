import java.util.Scanner;

public class Micro11 {
    public static void main(String[] args) {
        int numero;
        int x;
        Scanner s = new Scanner(System.in);

        System.out.printf("Digite um numero: ");
        numero = Integer.parseInt(s.nextLine());
        x = verifica(numero);

        if (x == 1) {
            System.out.printf("Numero positivo\n");
        }
        else
        if (x == 0) {
            System.out.printf("Zero\n");
        }
        else {
            System.out.printf("Numero negativo\n");
        }
    }

    public static int verifica(int n) {
        int res;

        if (n > 0) {
            res = 1;
        }
        else
        if (n < 0) {
            res = -1;
        }
        else {
            res = 0;
        }

        return res;
    }
}

