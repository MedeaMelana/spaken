package spaken.model;

public interface PointBinding<P> {
	public P getBindingFor(int i) throws UnboundPointException;

	public int createBinding(P p);

	public void setBindingFor(int index, P pos) throws UnboundPointException;
}
