import winnerpackage.*;

import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.Random;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.LongStream;
import java.util.stream.Stream;

public class prove {

    private static final String MALE_PATH = "./oscar_age_male.csv";
    private static final String FEMALE_PATH = "./oscar_age_female.csv";


	static Stream<String> oldWinners(Stream<Winner> inputStream){
		return inputStream
				.filter(w -> w.getWinnerAge() > 35) // filtering out every winner strictly older than 35
				.sorted(Comparator.comparing(Winner::getWinnerName))
				.map(Winner::getName); // sorting alphabetically by name
	}

	// EXERCISE 3
	static Stream<String> extremeWinners(Stream<Winner> input){
		return
				input
				.collect(Collectors.partitioningBy(w -> w.getWinnerAge() < 35)) // bipartition of the input stream by predicate isYoung()
				.values() // we just need the values of the Map returned by the collect above
				.stream() // effectively get the a Stream<List<Winner>>
				.map(winnerList -> winnerList.get(new Random().nextInt(winnerList.size()))) // get a random winner from each List

						// .map(winnerList -> winnerList.get( (int)(Math.random()*100) % winnerList .size())) // alternative way to get a random element
				.sorted(Comparator.comparing(Winner::getWinnerName)); // sorting alphabetically by name
	}

	public static void main(String[] args) {
		String[] paths = {MALE_PATH, FEMALE_PATH};

		List<Winner> winners = WinnerImpl.loadData(paths).collect(Collectors.toList());
        System.out.println("Contents of the long array created via comparison method:");
        (winners.stream().forEach(System.out::println);

	}

}
