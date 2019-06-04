import java.util.Scanner;

public class Micro06 {
    public static void main(String[] args) {
        int numero;
        Scanner s = new Scanner(System.in);

        System.out.printf("Digite um numero de 1 a 5: ");
        numero = Integer.parseInt(s.nextLine());
        
        switch (numero) {
            case 1:
                System.out.printf("Um\n");
                break;
            case 2:
                System.out.printf("Dois\n");
                break;
            case 3:
                System.out.printf("Tres\n");
                break;
            case 4:
                System.out.printf("Quatro\n");
                break;
            case 5:
                System.out.printf("Cinco\n");
                break;
            default:
                System.out.printf("Numero invalido!!!");
        }
    }
}
