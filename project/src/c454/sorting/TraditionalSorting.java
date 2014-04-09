package c454.sorting;

import java.util.Collections;
import java.util.Comparator;
import java.util.List;

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
}
