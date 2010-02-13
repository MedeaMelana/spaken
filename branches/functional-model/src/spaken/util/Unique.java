package spaken.util;

public final class Unique {
	private Unique() {}
	
	// mogelijk willen we slimme dingen doen met creatie
	public static Unique create() {
		return new Unique();
	}
}
