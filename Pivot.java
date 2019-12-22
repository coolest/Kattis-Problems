import java.util.Scanner;

public class Pivot {
	public static void main(String[] args) {
		Scanner ui = new Scanner(System.in);
		
		int answer = 0;
		int amount = ui.nextInt();
		int[] arr = new int[amount];
		boolean[] values = new boolean[amount];
		
		int n = Integer.MIN_VALUE;
		for (int i = 0; i < amount; i++) {
			int v = ui.nextInt();
			arr[i] = v;
			if (v > n) 
				n = v;
			else
				values[i] = true;
		}
		
		n = Integer.MAX_VALUE;
		for (int i = amount-1; i > -1; i--) {
			int v = arr[i];
			if (v <= n) {
				if (values[i] != true)
					answer++;
				n = v;
			}
		}
		
		System.out.println(answer);
		ui.close();
	}
}

