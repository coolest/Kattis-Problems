// https://open.kattis.com/problems/2048

import java.util.Scanner;
public class kattis_2048 {

	// Global Variables
	static int[][] numbers = new int[4][4];
	static int direction;
	static boolean flag1;
	static boolean flag2;

	// Method that gets the next number in an array and returns how many increments that number is from the index
	public static int[] getNextNumber(int index, int[] array) {
		if (index + 1 < 4)
			for (int i = index + 1; i < 4; i++)
				if (array[i] != 0)
					return new int[]{array[i], i-index};

		return null;
	}

	// Method that gets the 1D array
	public static int[] getArray(int index) {
		return new int[] {
			flag1 ? numbers[index][flag2 ? 0 : 3] : numbers[flag2 ? 0 : 3][index],
			flag1 ? numbers[index][flag2 ? 1 : 2] : numbers[flag2 ? 1 : 2][index],
			flag1 ? numbers[index][flag2 ? 2 : 1] : numbers[flag2 ? 2 : 1][index],
			flag1 ? numbers[index][flag2 ? 3 : 0] : numbers[flag2 ? 3 : 0][index],
		};
	}

	// Move numbers
	public static int[] format(int[] array) {
		boolean flag = false;

		do {
			flag = false;
			for (int i = 2; i > -1; i--)
				if(array[i] == 0 && array[i+1] != 0) {
					array[i] = array[i+1];
					array[i+1] = 0;
					flag = true;
				}
		}while(flag);

		return array;
	}

	// Main
	public static void main(String[] args) {
		Scanner ui = new Scanner(System.in);

		// Initialization
		for (int i = 0; i < 4; i++)
			for (int j = 0; j < 4; j++)
				numbers[i][j] = ui.nextInt();
		direction = ui.nextInt();
		flag1 = (direction == 0 || direction == 2);
		flag2 = (direction == 0 || direction == 1);

		// Loop
		int i = (direction == 1) ? 3 : 0;
		do {
			int[] array = getArray(i);
			int j = 0;
			boolean changed = false;

			do {
				int current = array[j];
				int[] next = getNextNumber(j, array);
				if (next == null)
					break;

				if(current == next[0] && !changed) {
					array[j+next[1]]*=2;
					array[j] = 0;
					changed = true;
				}else
					changed = false;

				j+=next[1];
			} while(j < 4);
			array = format(array);

			for (int k = 0; k < 4; k++)
				numbers[!flag1 ? flag2 ? k : 3-k : i][flag1 ? flag2 ? k : 3-k : i] = array[k];

			i = (direction == 1) ? i - 1 : i + 1;
		} while ((direction == 1) ? i > -1 : i < 4);

		for (int[] arr : numbers) {
			System.out.println();
			for (int n : arr)
				System.out.print(n + " ");
		}

		ui.close();
	}
}
