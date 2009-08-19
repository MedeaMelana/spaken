package spaken.model;

public interface ElementListener<E extends Element> {
	public void elementChanged(E e);
}
