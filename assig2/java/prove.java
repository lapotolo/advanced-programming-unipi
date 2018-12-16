
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

public class prove {

    private static final String MALE_PATH = "oscar_age_male.csv";
    private static final String FEMALE_PATH = "oscar_age_female.csv";

	// 1
	static Stream<String> oldWinners(Stream<Winner> inputStream){
		return inputStream
				.filter(w -> w.getWinnerAge() > 35) // filtering out every winner strictly older than 35
				.sorted(Comparator.comparing(Winner::getWinnerName)) // sorting alphabetically by name
				.map(Winner::getWinnerName);
	}

	// 2
	static Stream<String> extremeWinners(Stream<Winner> inputStream){
		return inputStream
				.collect(Collectors.partitioningBy(w -> w.getWinnerAge() < 35)) // bipartition of the input stream by predicate isYoung()
				.values() // we just need the values of the Map returned by the collect above
				.stream() // effectively get the a Stream<List<Winner>>
				.map(winnerList -> winnerList.get(new Random().nextInt(winnerList.size()))) // get a random winner from each List
				.map(Winner::getWinnerName);
				// .map(winnerList -> winnerList.get( (int)(Math.random()*100) % winnerList .size())) // alternative way to get a random element
				//.sorted(Comparator.comparing(Winner::getWinnerName)); // sorting alphabetically by name
	}

	// 3
	static Stream<Winner> multiAwardedFilm(Stream<Winner> input) {
		return InputStream
				.collect(Collectors.groupingBy(Winner::getFilmTitle)) // group winners by film
				.values() // we just need the values of the Map returned by the call to collect above
				.stream() // effectively get the a Stream<List<Winner>>
				.filter(filmsGroupedByName -> filmsGroupedByName.size() > 1) // keep in the stream just the "films" which has more than a winner
				.flatMap(list -> list.stream()) // flattening to Stream<Winner>
				.sorted(Comparator.comparing(Winner::getYear)) // sorting by year *** This is a stateful intermediate operation.
				.map(winner -> winner.getFilmTitle()) // Get a Stream<String> as requested
				.distinct(); // remove duplicates *** This is a stateful intermediate operation.			
	}
	Stream
	// 4
	// T = winner , U = String
	static <T, U> Stream runJobs(Stream< Function< Stream<T>, Stream<U> >> jobs, Collection<T> coll) {
		return coll.stream()
					.map()

	}

	public static void main(String[] args) {
		String[] paths = {MALE_PATH, FEMALE_PATH};
//		Collection<Winner> winners = WinnerImpl.processInputFile(MALE_PATH);
		Collection<Winner> winners = WinnerImpl.loadData(paths);

//		oldWinners(winners.stream()).forEach(System.out::println);
//		extremeWinners(winners.stream()).forEach(System.out::println);
		multiAwardedFilm(winners.stream()).forEach(System.out::println);

	}

}
