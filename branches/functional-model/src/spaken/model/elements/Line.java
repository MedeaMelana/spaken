package spaken.model.elements;

import spaken.model.*;

public class Line extends AbstractElement {

	private final Point p1, p2;

	Line(Point p1, Point p2) {
		if (p1 == p2) {
			throw new IllegalArgumentException(
					"Don't create line from two equal points.");
		}
		this.p1 = p1;
		this.p2 = p2;
	}

	public Point getP1() {
		return p1;
	}

	public Point getP2() {
		return p2;
	}

	public <Elem, Err extends Throwable> Elem visit(Spaken<Elem, Err> sp) throws Err {
		Elem r1 = p1.visit(sp);
		Elem r2 = p2.visit(sp);
		return sp.line(r1, r2);
	}

}
