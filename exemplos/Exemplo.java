import java.util.Scanner;

public class Exemplo {
    public static void doNothing() {
        System.out.printf("Ola mundo");
    }

    public static int soma(int a, int b) {
        return a + b;
    }

    public static void main(String[] args) {
        int x;

        x = soma(1, 2);
        doNothing();
    }
}
