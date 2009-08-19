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
		elements = new LinkedList<Element<?>>();
		exposed = false;
	}
	
	public void addElement(Element<?> element) {
		elements.add(element);
		element.theoremChanged(this);
		
//		List<Element<?>> deps = new LinkedList<Element<?>>();
//		element.collectDependencies(deps);
//		for (Element<?> e : deps) {
//			addElement(e);
//		}
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
