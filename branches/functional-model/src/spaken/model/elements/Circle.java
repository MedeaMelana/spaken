package spaken.model.elements;

import spaken.model.Spaken;

public class Circle extends AbstractElement {

	private final Point center, distFrom, distTo;

	Circle(Point center, Point distFrom, Point distTo) {
		this.center = center;
		this.distFrom = distFrom;
		this.distTo = distTo;
	}

	public Point getCenter() {
		return center;
	}

	public Point getDistFrom() {
		return distFrom;
	}

	public Point getDistTo() {
		return distTo;
	}

	public <Elem, Err extends Throwable> Elem visit(Spaken<Elem, Err> sp) throws Err {
		Elem c = center.visit(sp);
		Elem df = distFrom.visit(sp);
		Elem dt = distTo.visit(sp);
		return sp.circle(c, df, dt);
	}


}
