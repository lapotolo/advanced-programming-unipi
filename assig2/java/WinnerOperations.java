import java.util.Arrays;
import java.util.Collection;
import java.util.Comparator;
import java.util.List;
import java.util.Random;
import java.util.function.Function;
import java.util.stream.Collectors;

import java.util.stream.LongStream;
import java.util.stream.Stream;
import java.util.ArrayList;
import java.util.*;

public class WinnerOperations {

    private static final String MALE_PATH = "oscar_age_male.csv";
    private static final String FEMALE_PATH = "oscar_age_female.csv";

	// 1
	static Stream<String> oldWinners(Stream<Winner> inputStream){
		return inputStream
				.filter(w -> w.getWinnerAge() > 35) // filtering out every winner strictly younger than 35
				.sorted(Comparator.comparing(Winner::getWinnerName)) // sorting alphabetically by winner name
				.map(Winner::getWinnerName); // transforming into a Stream<String> where each String is the name of a winner as requested by the implemented method
	}

	// 2 
	static Stream<String> extremeWinners(Stream<Winner> inputStream){
		// I need to collect the input stream to reuse it multiple times. 
		Collection<Winner> winners = inputStream.collect(Collectors.toList());
		int minAge = winners.stream().mapToInt(Winner::getWinnerAge).min().orElse(Integer.MIN_VALUE); // getting the min age in the db
		int maxAge = winners.stream().mapToInt(Winner::getWinnerAge).max().orElse(Integer.MAX_VALUE); // getting the max age in the db
		return winners.stream()
				.filter(w -> (w.getWinnerAge() == minAge) || (w.getWinnerAge() == maxAge)) // filtering out every winner with age != minAge or maxAge
				.map(Winner::getWinnerName) // going from Stream<Winner> to a Stream<String>
				.sorted(Comparator.reverseOrder()); // sorting in reversed lexicographical order
	}

	// 3
	static Stream<String> multiAwardedFilm(Stream<Winner> inputStream) {
		return inputStream
				.collect(Collectors.groupingBy(Winner::getFilmTitle)) // group winners by film
				.values() // we just need the values of the Map returned by the call to collect above
				.stream() // effectively get the a Stream<List<Winner>>
				.filter(filmsGroupedByName -> filmsGroupedByName.size() > 1) // keep in the stream just the "films" which has more than a winner
				.flatMap(list -> list.stream()) // flattening to Stream<Winner>
				.sorted(Comparator.comparing(Winner::getYear)) // sorting by year *** This is a stateful intermediate operation.
				.map(winner -> winner.getFilmTitle()) // Get a Stream<String> as requested
				.distinct(); // remove duplicates *** This is a stateful intermediate operation.			
	}

	// 4
	static <T, U> Stream<U> runJobs(Stream< Function< Stream<T>, Stream<U> >> jobs, Collection<T> coll) {
		return jobs
				.map(job -> job.apply(coll.stream()))
				.flatMap(Function.identity()); // needed to pass from a Stream<Stream<String>> to a Stream<String>
	}	
	// both flatMap(Function.identity()) and flatMap(resultStream -> resultStream )
	// give the same result. The first one gives reduced space cost.
	// source: https://stackoverflow.com/questions/28032827/java-8-lambdas-function-identity-or-t-t 
	
	// 5
	public static void main(String[] args) {		
		String[] paths = {MALE_PATH, FEMALE_PATH};
		Collection<Winner> winners = WinnerImpl.loadData(paths);
		
		Function<Stream<Winner>, Stream<String>> fun1 = WinnerOperations::oldWinners;	
		Function<Stream<Winner>, Stream<String>> fun2 = WinnerOperations::extremeWinners;
		Function<Stream<Winner>, Stream<String>> fun3 = WinnerOperations::multiAwardedFilm;
		runJobs(Stream.of(fun1, fun2, fun3), winners)
			.collect(Collectors.toList()) // returns a List<String> but this is not needed to just print the results as requested
			.forEach(System.out::println);
	}
}