package spaken.model;

import java.util.List;

public class ListPointBinding<P> implements PointBinding<P> {

	private List<P> list;
	
	public ListPointBinding(List<P> list) {
		this.list = list;
	}
	
	public void setBindingFor(int i, P p) throws UnboundPointException {
		try {
			list.set(i, p);
		} catch (IndexOutOfBoundsException e) {
			throw new UnboundPointException(i);
		}
	}
	
	public int createBinding(P p) {
		int i = list.size();
		list.add(i, p);
		return i;
	}
	
	public P getBindingFor(int i) throws UnboundPointException {
		try {
			return list.get(i);
		} catch (IndexOutOfBoundsException e) {
			throw new UnboundPointException(i);
		}
	}

}
