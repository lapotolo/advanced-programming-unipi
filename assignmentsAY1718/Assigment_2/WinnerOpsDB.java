import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.Random;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.LongStream;
import java.util.stream.Stream;


public class WinnerOpsDB {
	
    private static final String MALE_PATH = "input_files/oscar_age_male.csv";
    private static final String FEMALE_PATH = "input_files/oscar_age_female.csv";

	// EXERCISE 1
	static Stream<Winner> loadData(String[] paths) {
		return Stream.of(paths)
				.flatMap(p -> { // flattening 
					try {
						return Files.lines(Paths.get(p)) // this one can throw IOException
								.skip(1) // skip header line
								.map(word -> word.split(",")) // split every line at ","
								.map(win -> (Winner) new WinnerImp(Integer.parseInt(win[1]), Integer.parseInt(win[2]), win[3].replace("\"", ""), win[4].replace("\"", ""))); // cast needed to match signature	
					} catch(IOException e) {
						throw new RuntimeException();
					}
				});
	}

	// EXERCISE 2
	static Stream<Winner> youngWinners(Stream<Winner> input){
		return input
				.filter(w -> w.getWinnerAge() < 35) // filter out all the winner older than 35
				.sorted(Comparator.comparing(Winner::getWinnerName)); // sorting alphabetically by name
	}

	// EXERCISE 3
	static Stream<Winner> extremeWinners(Stream<Winner> input){
		return
				input
				.collect(Collectors.partitioningBy(w -> w.getWinnerAge() < 35)) // bipartition of the input stream by predicate isYoung()
				.values() // we just need the values of the Map returned by the collect above
				.stream() // effectively get the a Stream<List<Winner>>
				.map(winnerList -> winnerList.get(new Random().nextInt(winnerList.size()))) // get a random winner from each List
				//				.map(winnerList -> winnerList.get( (int)(Math.random()*100) % winnerList .size())) // alternative way to get a random element
				.sorted(Comparator.comparing(Winner::getWinnerName)); // sorting alphabetically by name
	}

	// EXERCISE 4
	static Stream<String> multiAwardedPerson(Stream<Winner> input){
		return input
				.collect(Collectors.groupingBy(Winner::getWinnerName)) // group winners by name
				.values() // we just need the values of the Map returned by the call to collect above
				.stream() // effectively get the a Stream<List<Winner>>
				.filter(listOfWinnersGroupedByName -> listOfWinnersGroupedByName.size() > 1) // keep in the stream just the Winners who won more than one time
				.map(w -> w.get(0).getWinnerName())
				.sorted(); //  *** sort string in alphabetical order
	}

	// EXERCISE 5
	static Stream<String> multiAwardedFilm(Stream<Winner> input) {
		return input
				.collect(Collectors.groupingBy(Winner::getFilmTitle)) // group winners by film
				.values() // we just need the values of the Map returned by the call to collect above
				.stream() // effectively get the a Stream<List<Winner>>
				.filter(filmsGroupedByName -> filmsGroupedByName.size() > 1) // keep in the stream just the "films" which has more than a winner
				.flatMap(list -> list.stream()) // flattening to Stream<Winner>
				.sorted(Comparator.comparing(Winner::getYear)) // sorting by year *** This is a stateful intermediate operation.
				.map(winner -> winner.getFilmTitle()) // Get a Stream<String> as requested
				.distinct(); // remove duplicates *** This is a stateful intermediate operation.			
	}

	// *****************************************************************************************************
	// *																								   *
	// *  NOTE ON MY PARALLEL IMPLEMENTATION:                                                              *
	// *  In my implementation I transform the sequential stream given in input into a parallel            *
	// *  stream and again into a sequential one any time I need to use a stateful intermediate operation. *
	// *  So in some parts the code is a little clumsy (see parallel version of exercise 5).               *
	// *  Anyway if the results given by Parallel methods would be tested/exploited using                  *
	// *  a .forEachOrdered(...) (it works even if called outside the function body),                      *
	// *  the transformation done inside the function would have not been necessary.                       *
	// *  I preferred this solution because I have no knowledge about how this code will be used           *
	// *  even if an hypothetical tester could check if the returned stream is parallel via the function   *
	// *  isParallel() and choose to run a forEachOrdered(...) if IsParallel() gives true else             *
	// *  to run a forEach(...) if isParallel gives false.                                                 *
	// *                                                                                                   *
	// *****************************************************************************************************


	//	EXERCISE 6
	// 2P
	static Stream<Winner> youngWinnersParallel(Stream<Winner> input){
		return input
				.parallel() // the input stream becomes a parallel stream
				.filter(w -> w.getWinnerAge() < 35)
				.sequential() // (re)trasformation of the stream into a sequential one before a stateful operation
				.sorted(Comparator.comparing(Winner::getWinnerName)); // ***This is a stateful intermediate operation. i.e. sorting
	}

	// 3P
	static Stream<Winner> extremeWinnersParallel(Stream<Winner> input){
		return
				input
				.parallel() // the input stream becomes a parallel stream
				.collect(Collectors.partitioningBy(w -> w.getWinnerAge() < 35)) // bipartition of the input stream by predicate isYoung()
				.values() // we just need the values of the Map returned by the collect above
				.parallelStream() // effectively get a Stream<List<Winner>>
				.map(winnerList -> winnerList.get(new Random().nextInt(winnerList.size()))) // get just one random winner from each List (there are two list)
				.sequential() // (re)trasformation of the stream into a sequential one before a stateful operation
				.sorted(Comparator.comparing(Winner::getWinnerName)); // sorting alphabetically by name	
	}

	// 4P
	static Stream<String> multiAwardedPersonParallel(Stream<Winner> input){
		return input
				.parallel() // MAYBE
				.collect(Collectors.groupingBy(Winner::getWinnerName))
				.values()
				.parallelStream()
				.filter(winnersGroupedByName -> winnersGroupedByName.size() > 1)
				.map(w -> w.get(0).getWinnerName())
				.sequential() // (re)trasformation of the stream into a sequential one before a stateful operation
				.sorted(); // sort string in alphabetical order
	}

	// 5P
	static Stream<String> multiAwardedFilmParallel(Stream<Winner> input) {
		return input
				.parallel()
				.collect(Collectors.groupingBy(Winner::getFilmTitle))
				.values()
				.parallelStream()
				.filter(filmsGroupedByName -> filmsGroupedByName.size() > 1)
				.flatMap(list -> list.parallelStream()) // here a sequential stream is created since the next operation is a sort
				.sorted(Comparator.comparing(Winner::getYear)) // ***stateful
				.parallel()
				.map(winner -> winner.getFilmTitle())
				.sequential()
				.distinct(); // ***stateful	
	}

	static <T, U> long measure(Function<Stream<T>, Stream<U>> f, Stream<T> s1 ) {
		Stream<U> s2 = f.apply(s1);
		long start = System.nanoTime(); 
		s2.collect(Collectors.toList());
		return System.nanoTime() - start;
	}

	// OPTIONAL. JUSTIFY YOUR CHOICE!
	static <T, U> LongStream runJobs(Stream<Function<Stream<T>,Stream<U>>> jobs, Stream<T> s) {
        List<T> list = s.collect(Collectors.toList());
		return jobs
				.mapToLong(job -> measure(job, list.stream()));
	}	

	public static long[] comparison(Stream<Winner> stream) {
		long[] measures = new long[11];
		// these return Stream<Winner>
		Function<Stream<Winner>, Stream<Winner>> functionEx1 = WinnerOpsDB::youngWinners;
		Function<Stream<Winner>, Stream<Winner>> functionEx2 = WinnerOpsDB::extremeWinners;
		Function<Stream<Winner>, Stream<Winner>> functionEx1Par = WinnerOpsDB::youngWinnersParallel;
		Function<Stream<Winner>, Stream<Winner>> functionEx2Par = WinnerOpsDB::extremeWinnersParallel;
		// these return Stream<String>
		Function<Stream<Winner>, Stream<String>> functionEx3 = WinnerOpsDB::multiAwardedPerson;
		Function<Stream<Winner>, Stream<String>> functionEx4 = WinnerOpsDB::multiAwardedFilm;
		Function<Stream<Winner>, Stream<String>> functionEx3Par = WinnerOpsDB::multiAwardedPersonParallel;
		Function<Stream<Winner>, Stream<String>> functionEx4Par = WinnerOpsDB::multiAwardedFilmParallel;
		
		// I need to store the input stream because it will be consumed two times
		List<Winner> temp = stream.collect(Collectors.toList()); // need to save the stream in input in a collection to multiplex
		long[] arrayWinner = runJobs(Stream.of(functionEx1, functionEx2, functionEx1Par, functionEx2Par), temp.stream()).toArray();
		long[] arrayString = runJobs(Stream.of(functionEx3, functionEx4, functionEx3Par, functionEx4Par), temp.stream()).toArray();
		measures[0] = -1; 
		measures[1] = -1; 
		measures[2] = arrayWinner[0];
		measures[3] = arrayWinner[1];
		measures[4] = arrayString[0];
		measures[5] = arrayString[1];
		measures[6] = -1;
		measures[7] = arrayWinner[2];
		measures[8] = arrayWinner[3];
		measures[9] = arrayString[2];
		measures[10] = arrayString[3];
		return measures;
	}


	public static void main(String[] args) {
		String[] paths = {MALE_PATH, FEMALE_PATH};

		System.out.println("Exercise 2 seq: " + measure(WinnerOpsDB::youngWinners ,loadData(paths)) / 1000000000.0+" seconds");
		System.out.println("Exercise 2 par: " + measure(WinnerOpsDB::youngWinnersParallel ,loadData(paths)) / 1000000000.0 + " seconds");

		System.out.println("");

		System.out.println("Exercise 3 seq: " + measure(WinnerOpsDB::extremeWinners ,loadData(paths)) / 1000000000.0+" seconds");
		System.out.println("Exercise 3 par: " + measure(WinnerOpsDB::extremeWinnersParallel ,loadData(paths)) / 1000000000.0 + " seconds");

		System.out.println("");

		System.out.println("Exercise 4 seq: " + measure(WinnerOpsDB::multiAwardedPerson ,loadData(paths)) / 1000000000.0+" seconds");
		System.out.println("Exercise 4 par: " + measure(WinnerOpsDB::multiAwardedPersonParallel ,loadData(paths)) / 1000000000.0 + " seconds");

		System.out.println("");

		System.out.println("Exercise 5 seq: " + measure(WinnerOpsDB::multiAwardedFilm ,loadData(paths)) / 1000000000.0+" seconds");
		System.out.println("Exercise 5 par: " + measure(WinnerOpsDB::multiAwardedFilmParallel ,loadData(paths)) / 1000000000.0 + " seconds");
		System.out.println("");


        List<Winner> winners = loadData(paths).collect(Collectors.toList());
        System.out.println("Contents of the long array created via comparison method:");
        Arrays.stream(comparison(winners.stream())).forEach(System.out::println);

	}

}