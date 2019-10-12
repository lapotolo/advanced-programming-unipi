
public class WinnerImp implements Winner {
	private int year;
	private int age;
	private String name;
	private String title;
	
	public WinnerImp(int y, int a, String n, String t) {
		this.year = y;
		this.age = a;
		this.name = n;
		this.title = t;
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
}