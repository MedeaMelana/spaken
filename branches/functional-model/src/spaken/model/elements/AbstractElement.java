package spaken.model.elements;

import spaken.model.Element;
import spaken.model.util.Unique;

public abstract class AbstractElement implements Element {
	private final Unique id;
	
	public AbstractElement() {
		id = Unique.create();
	}
	
	public Unique getId() {
		return id;
	}
}
