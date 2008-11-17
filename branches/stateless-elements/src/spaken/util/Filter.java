package spaken.util;

public interface Filter<A,B> {

	public boolean accepts(A a);
	public B map(A a);

}
