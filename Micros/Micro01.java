import java.util.Scanner;

public class Micro01 {
	public static void main(String[] args) {
		float cel, far;
		Scanner s = new Scanner(System.in);

		System.out.printf("Tabela de conversao: Celcius -> Farenheit\n");
		System.out.printf("Digite a temperatura em Celcius:");
		cel = Float.parseFloat(s.nextLine());
<<<<<<< HEAD
		far = (9.0*cel+160.0)/5.0;
=======
		far = (9*cel+160)/5;
>>>>>>> 37632df0e16531dc80ecaea8398d764bfec8f142
		System.out.printf("A nova temperatura e: %f F\n", far);
	}
}
