import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;

import java.util.Collection;
import java.util.stream.Collectors;
import java.util.stream.LongStream;
import java.util.stream.Stream;

import java.util.ArrayList;
import java.util.List;

public class WinnerImpl implements Winner {
	private int year;
	private int age;
	private String name;
	private String title;

	public WinnerImpl(int year, int age, String name, String title) {
		this.year = year;
		this.age = age;
		this.name = name;
		this.title = title;
	}

	@Override
	public int getYear() {
		return this.year;
	}

	@Override
	public int getWinnerAge() {
		return this.age;
	}

	@Override
	public String getWinnerName() {
		return this.name;
	}

	@Override
	public String getFilmTitle() {
		return this.title;
	}

	public String toString() {
		return this.getYear() +", "+ this.getWinnerAge() +", " + this.getWinnerName() +", "+ this.getFilmTitle();
	}

    static Collection<Winner> loadData(String[] paths) {
		return Stream.of(paths)
		// flattening to a Stream<Winner> from a  of 1 dimension of winners since we are loading more than one file
			.flatMap(p -> { 
				try {
					return Files.lines(Paths.get(p)) // this one can throw IOException
						.skip(1) // skip header line
						.map(word -> word.split(",")) // split every line at ","
						.map(win -> (Winner) new WinnerImpl(Integer.parseInt(win[1]), Integer.parseInt(win[2]), win[3].replace("\"", ""), win[4].replace("\"", ""))); // cast needed to match signature
				} catch(IOException e) {
					System.out.println("ciao merda!");
					throw new RuntimeException();
				}
			})
			.collect(Collectors.toList());
	}


	// LOADING FILES FUNCTIONALLY
	// alternative way to open a single file
/*
	static Collection<Winner> processInputFile(String inputFilePath) {
		List<Winner> inputAsList = new ArrayList<Winner>();
		try {
		  	InputStream inputFS = new FileInputStream(new File(inputFilePath));
		  	BufferedReader br = new BufferedReader(new InputStreamReader(inputFS));
		  	// skip the header of the csv
		  	inputAsList = br.lines()
					.skip(1)
					.map(word -> word.split(","))
					.map(win -> (Winner) new WinnerImpl(Integer.parseInt(win[1]), Integer.parseInt(win[2]), win[3].replace("\"", ""), win[4].replace("\"", ""))) // cast needed to match signature					
				  	.collect(Collectors.toList());
		  	br.close();
		} catch (Exception e) {	  
			System.out.println("eccezione lanciata!");
		}
		return inputAsList ;
	}
*/

}