package spaken.model.elements;

import java.util.HashMap;
import java.util.Map;

import spaken.model.*;
import spaken.util.Unique;

public class Construct implements Spaken<Element, ClassCastException> {
	
	// TODO overload stuff to get a type safe constructor
	
	private Map<Unique, Point> knownPoints;
	// TODO cache ook andere elementen dan Point (niet strikt noodzakelijk; scheelt wel geheugen)
	
	public Construct() {
		knownPoints = new HashMap<Unique, Point>();
	}
	
	public Element circle(Element center, Element distFrom, Element distTo) throws ClassCastException {
		return new Circle((Point) center, (Point) distFrom, (Point) distTo);
	}

	public Element getPoint(Element points, int i) throws ClassCastException {
		return ((Points) points).getPoint(i);
	}

	public Element intersectCC(Element circle1, Element circle2) throws ClassCastException {
		return new IntersectCC((Circle) circle1, (Circle) circle2);
	}

	public Element intersectLC(Element line, Element circle) throws ClassCastException {
		return new IntersectLC((Line) line, (Circle) circle);
	}

	public Element intersectLL(Element line1, Element line2) throws ClassCastException {
		return new IntersectLL((Line) line1, (Line) line2);
	}

	public Element line(Element p1, Element p2) throws ClassCastException {
		return new Line((Point) p1, (Point) p2);
	}

	public Element makePoint(Unique id, Pos pos) throws ClassCastException {
		if (id == null) {
			return new AssumedPoint(pos);
		}
		
		Point p = knownPoints.get(id);
		if (p == null) {
			p = new AssumedPoint(pos);
			knownPoints.put(id, p);
		}
		return p;
	}


}
