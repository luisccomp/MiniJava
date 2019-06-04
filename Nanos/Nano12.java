public class Nano12 {
    public static void main(String[] args) {
        int n, m, x;
        n = 1;
        m = 2;
        x = 5;
        while(x>n){
            if(n==m) {
                System.out.printf("%d\n", n);
            }
            else {
                System.out.printf("%d\n", 0);
            }
            x = x - 1;
        }
    }
}

