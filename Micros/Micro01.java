import java.util.Scanner;

public class Micro01 {
	public static void main(String[] args) {
		float cel, far;
		Scanner s = new Scanner(System.in);

		System.out.printf("Tabela de conversao: Celcius -> Farenheit\n");
		System.out.printf("Digite a temperatura em Celcius:");
		cel = Float.parseFloat(s.nextLine());
		far = (9*cel+160)/5;
		System.out.printf("A nova temperatura e: %f F\n", far);
	}
}
