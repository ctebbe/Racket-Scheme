//package c454.sorting;

import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.*;
import java.io.*;

/**
 * Created by ctebbe on 3/31/14.
 */
public class TraditionalSorting {
    public static void sort(List<Integer> list) {
        Collections.sort(list, new Comparator<Integer>() {
            @Override
            public int compare(Integer o1, Integer o2) {
                return o1.compareTo(o2);
            }
        });
    }

    public static void main(String[] args) throws FileNotFoundException {
        Scanner scanner = new Scanner(new File("numbers.txt"));
        List<Integer> list = new ArrayList<>();
        while(scanner.hasNextInt()) {
            list.add(scanner.nextInt());
        }

        TraditionalSorting.sort(list);
        for(Integer i : list) System.out.print(i + " ");
        System.out.println();
    }
}
