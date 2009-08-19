package spaken.model;

import java.awt.Graphics2D;
import java.util.LinkedList;
import java.util.List;

import spaken.model.elements.AssumedPoint;
import spaken.util.ListWithoutDuplicates;

public class Theorem {

	private List<Element<?>> elements;

	private boolean exposed;

	public Theorem() {
		this(new LinkedList<Element<?>>());
	}

	private Theorem(List<Element<?>> elements) {
		this.elements = elements;
		this.exposed = false;
	}

	public Theorem duplicate() {
		// TODO We need to duplicate the underlying elements, maintaining
		// sharing between them. How?
		LinkedList<Element<?>> copyElems = new LinkedList<Element<?>>();
		copyElems.addAll(elements);

		Theorem copy = new Theorem(copyElems);
		copy.exposed = exposed;

		return copy;
	}

	public void addElement(Element<?> element) {
		elements.add(element);
		element.theoremChanged(this);

		// List<Element<?>> deps = new LinkedList<Element<?>>();
		// element.collectDependencies(deps);
		// for (Element<?> e : deps) {
		// addElement(e);
		// }
	}

	public List<AssumedPoint> getAssumptions() {
		ListWithoutDuplicates<AssumedPoint> points = new ListWithoutDuplicates<AssumedPoint>();
		for (Element<?> element : elements) {
			element.collectAssumptions(points);
		}

		return points;

	}

	public boolean isExposed() {
		return exposed;
	}

	public void setExposed(boolean exposed) {
		this.exposed = exposed;
	}

	public void draw(Graphics2D g, int pixelSize) {
		for (Element<?> element : elements) {
			if (exposed || element.isExported()) {
				element.draw(g, pixelSize);
			}
		}
	}

}
