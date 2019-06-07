import java.util.Scanner;

public class Micro07 {
    public static void main(String[] args) {
        int programa, numero;
        char opc;
        Scanner s = new Scanner(System.in);

        programa = 1;

        while (programa == 1) {
            System.out.printf("Digite um numero: ");
            numero = Integer.parseInt(s.nextLine());

            if (numero > 0) {
                System.out.printf("Positivo\n");
            } else {
                if (numero == 0) {
                    System.out.printf("O numero e igual a 0\n");
                }

                if (numero < 0) {
                    System.out.printf("Negativo\n");
                }
            }

            System.out.printf("Deseja finalizar? (S/N) ");
            opc = s.nextLine().charAt(0);

            if (opc == 'S') {
                programa = 0;
            }
        }
    }
}
