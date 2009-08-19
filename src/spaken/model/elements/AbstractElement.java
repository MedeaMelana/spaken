package spaken.model.elements;

import java.util.*;

import spaken.model.*;

public abstract class AbstractElement<E extends Element> implements Element<E> {

	private Theorem theorem;
	private List<ElementListener<? super E>> elementListeners;
	private boolean export;
	
	public AbstractElement() {
		export = false;
		elementListeners = new LinkedList<ElementListener<? super E>>();
	}
	
	public void addElementListener(ElementListener<? super E> e) {
		elementListeners.add(e);
	}

	public void removeElementListener(ElementListener<? super E> e) {
		elementListeners.add(e);
	}
	
	// TODO elem hoort eigenlijk altijd this te zijn, maar daar willen de generics niet aan.
	protected void notifyElementListeners(E elem) {
		for (ElementListener<? super E> listener : elementListeners) {
			listener.elementChanged(elem);
		}
	}
	
	protected Collection<ElementListener<? super E>> getElementListeners() {
		return Collections.unmodifiableCollection(elementListeners);
	}
	
	public Theorem getTheorem() {
		return theorem;
	}
	
	public void theoremChanged(Theorem theorem) {
		this.theorem = theorem;
	}
	
	public boolean isExported() {
		return export;
	}
	
	public void setExported(boolean export) {
		this.export = export;
	}

}
