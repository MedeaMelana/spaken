package spaken.util;

public class ClassFilter implements Filter<Object> {

	private Class<?> cl;

	public ClassFilter(Class<?> cl) {
		this.cl = cl;
	}

	public boolean accepts(Object t) {
		return cl.isAssignableFrom(t.getClass());
	}

}
