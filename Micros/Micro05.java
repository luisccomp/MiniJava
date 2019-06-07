import java.util.Scanner;

public class Micro05 {
    public static void main(String[] args) {
<<<<<<< HEAD
        /*String nome;
=======
        String nome;
>>>>>>> 37632df0e16531dc80ecaea8398d764bfec8f142
        char sexo;
        int x, h, m;
        Scanner s = new Scanner(System.in);

        for (x = 0; x < 5; x ++) {
            System.out.printf("Digite o nome: ");
            nome = s.nextLine();
            System.out.printf("H - homem ou M - mulher: ");
            sexo = s.nextLine().charAt(0);

            switch (sexo) {
                case 'H':
                    h = h + 1;
                    break;
                case 'M':
                    m = m + 1;
                    break;
                default:
                    System.out.printf("Sexo so pode ser H ou M\n");
            }
        }

        System.out.printf("Foram inseridos %d homens", h);
<<<<<<< HEAD
        System.out.printf("Foram inseridos %d mulheres", m);*/
        String nome;
        char sexo;
        int x, h, m;
        Scanner s = new Scanner(System.in);

        for (x = 0; x < 5; x ++) {
            System.out.printf("Digite o nome: ");
            nome = s.nextLine();
            System.out.printf("H - homem ou M - mulher: ");
            sexo = s.nextLine().charAt(0);
            
            switch(sexo) {
                case 'H':
                    h = h + 1;
                    break;
                case 'M':
                    m = m + 1;
                    break;
                default:
                    System.out.printf("Sexo so pode ser M ou H\n");
            }
        }
=======
        System.out.printf("Foram inseridos %d mulheres", m);
>>>>>>> 37632df0e16531dc80ecaea8398d764bfec8f142
    }
}
