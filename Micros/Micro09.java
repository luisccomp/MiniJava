import java.util.Scanner;

public class Micro09 {
    public static void main(String[] args) {
        float preco, venda, novoPreco;
        Scanner s = new Scanner(System.in);

        System.out.printf("Digite um numero: ");
        preco = Float.parseFloat(s.nextLine());
        System.out.printf("Digite a venda: ");
        venda = Float.parseFloat(s.nextLine());

        if ((venda < 500.0) || (preco < 30.0)) {
            novoPreco = preco + 10.0 / 100.0 * preco;
        }
        else {
            if ((venda >= 500.0 && venda < 1200.0) || (preco >= 30.0 && preco < 80.0)) {
                novoPreco = preco + 15.0 / 100.0 * preco;
            }
            else {
                if (venda >= 1200.0 || preco >= 80.0) {
                    novoPreco = preco - 20.0 / 100.0 * preco;
                }
            }
        }

        System.out.printf("O novo preco e %f\n", preco);
    }
}
