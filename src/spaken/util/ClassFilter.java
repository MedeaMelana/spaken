package spaken.util;

public class ClassFilter<C> implements Filter<Object,C> {

	private Class<C> cl;

	public ClassFilter(Class<C> cl) {
		this.cl = cl;
	}

	public boolean accepts(Object object) {
		return cl.isAssignableFrom(object.getClass());
	}
	
	public C map(Object object) {
	  return cl.cast(object);
	}

}
