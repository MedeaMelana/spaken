package spaken.model.elements.intersections;

import spaken.model.*;
import spaken.model.elements.*;

public class Intersections {

	public static Point intersect(Theorem theorem, Line l1, Line l2) {
		return new LineLineIntersectionPoint(theorem, l1, l2);
	}

	public static Point[] intersect(Theorem theorem, Circle c1, Circle c2) {
		return new Point[] {
				new CircleCircleIntersectionPoint(theorem, c1, c2, 1),
				new CircleCircleIntersectionPoint(theorem, c1, c2, -1) };
	}

	public static Point[] intersect(Theorem theorem, Line l, Circle c) {
		return new LineCircleIntersectionPoint[] {
				new LineCircleIntersectionPoint(theorem, l, c, 1),
				new LineCircleIntersectionPoint(theorem, l, c, -1) };
	}

	public static Point[] intersect(Theorem theorem, Circle c, Line l) {
		return intersect(theorem, l, c);
	}

	/**
	 * Determine all intersection points between elements <t>e1</tt> and <tt>e2</tt>.
	 * 
	 * @param e1
	 * @param e2
	 * @return
	 */
	public static Point[] intersections(Theorem theorem, Element e1, Element e2) {
		if (e1 instanceof Line) {
			if (e2 instanceof Line) {
				return new Point[] { intersect(theorem, (Line) e1, (Line) e2) };
			} else if (e2 instanceof Circle) {
				return intersect(theorem, (Line) e1, (Circle) e2);
			}
		} else if (e1 instanceof Circle) {
			if (e2 instanceof Circle) {
				return intersect(theorem, (Circle) e1, (Circle) e2);
			} else if (e2 instanceof Line) {
				return intersect(theorem, (Circle) e1, (Line) e2);
			}
		}

		return new Point[0];
	}

}
