import java.util.Scanner;

public class Micro09 {
    public static void main(String[] args) {
        float preco, venda, novoPreco;
        Scanner s = new Scanner(System.in);

        System.out.printf("Digite um numero: ");
        preco = Float.parseFloat(s.nextLine());
        System.out.printf("Digite a venda: ");
        venda = Float.parseFloat(s.nextLine());

<<<<<<< HEAD
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
=======
        if ((venda < 500) || (preco < 30)) {
            novoPreco = preco + 10 / 100 * preco;
        }
        else {
            if ((venda >= 500 && venda < 1200) || (preco >= 30 && preco < 80)) {
                novoPreco = preco + 15 / 100 * preco;
            }
            else {
                if (venda >= 1200 || preco >= 80) {
                    novoPreco = preco - 20 / 100 * preco;
>>>>>>> 37632df0e16531dc80ecaea8398d764bfec8f142
                }
            }
        }

        System.out.printf("O novo preco e %f\n", preco);
    }
}
