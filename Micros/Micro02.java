import java.util.Scanner;

public class Micro02 {
    public static void main(String[] args) {
        int num1, num2;
        Scanner s = new Scanner(System.in);

        System.out.printf("Digite o primeiro numero: ");
        num1 = Integer.parseInt(s.nextLine());
        System.out.printf("Digite o segundo numero: ");
        num2 = Integer.parseInt(s.nextLine());

        if (num1 > num2) {
            System.out.printf("O primeiro numero %d e maior que o segundo %d\n", num1, num2);
        } else {
            System.out.printf("O segundo numero %d e maior que o primeiro %d\n", num2, num1);
        }
    }
}
