

import java.util.Scanner;

/*          Insertion Sort	   */

public class insertion_Sort {

    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        int n=sc.nextInt();
        int arr[] = new int[n];
        int i;
        for(i=0;i<n;i++)
            arr[i]=sc.nextInt();

        insertionSort(arr,n);

        for(i=0;i<n;i++)
            System.out.print(arr[i]+" ");

    }

    private static void insertionSort(int arr[],int n)
    {
        //  for n elements , maximum (n-1) passes are required
        int no_of_swaps = 0;
        int i,j;

        for(i=1;i<n;i++)
        {
            int marked = arr[i];
            j=i-1;
            while(j>=0 && marked<arr[j])
            {
                arr[j+1]=arr[j];
                j--;
                no_of_swaps++;
            }
            j++;
            arr[j]=marked;

        }

        System.out.println("No of swaps = "+no_of_swaps);
    }
}

