package spaken.model;

public interface Dependency<E extends Element> {
	public void elementChanged(E e);
}
