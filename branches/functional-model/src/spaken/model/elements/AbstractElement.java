package spaken.model.elements;

import spaken.util.Unique;

public abstract class AbstractElement implements Element {
	private final Unique id;

	public AbstractElement() {
		id = Unique.create();
	}

	public Unique getId() {
		return id;
	}

	public boolean equals(Object other) {
		/*
		 * Compares references. For Elements other than AssumedPoints, this may
		 * be overridden by a smarter check that compares the elements on which
		 * it depends.
		 */
		if (other != null && other instanceof Element) {
			return id.equals(((Element) other).getId());
		}
		return false;
	}
}
