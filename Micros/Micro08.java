import java.util.Scanner;

public class Micro08 {
    public static void main(String[] args) {
        int numero;
        Scanner s = new Scanner(System.in);

        numero = 1;

        while (numero != 0) {
            System.out.printf("Digite um numero: ");
            numero = Integer.parseInt(s.nextLine());

            if (numero > 10) {
                System.out.printf("O numero %d e maior que 10", numero);
            } else {
                System.out.printf("O numero %d e menor que 10", numero);
            }
        }
    }
}
