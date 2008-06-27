package spaken.model.intersections;

import spaken.model.*;

public class Intersections {

	public static Point intersect(Line l1, Line l2) {
		return new LineLineIntersectionPoint(l1, l2);
	}

	public static Point[] intersect(Circle c1, Circle c2) {
		return new Point[] { new CircleCircleIntersectionPoint(c1, c2, 1),
				new CircleCircleIntersectionPoint(c1, c2, -1) };
	}

	public static Point[] intersect(Line l, Circle c) {
		return new LineCircleIntersectionPoint[] {
				new LineCircleIntersectionPoint(l, c, 1),
				new LineCircleIntersectionPoint(l, c, -1) };
	}

	public static Point[] intersect(Circle c, Line l) {
		return intersect(l, c);
	}

	/**
	 * Determine all intersection points between elements <t>e1</tt> and <tt>e2</tt>.
	 * 
	 * @param e1
	 * @param e2
	 * @return
	 */
	public static Point[] intersections(Element e1, Element e2) {
		if (e1 instanceof Line) {
			if (e2 instanceof Line) {
				return new Point[] { intersect((Line) e1, (Line) e2) };
			} else if (e2 instanceof Circle) {
				return intersect((Line) e1, (Circle) e2);
			}
		} else if (e1 instanceof Circle) {
			if (e2 instanceof Circle) {
				return intersect((Circle) e1, (Circle) e2);
			} else if (e2 instanceof Line) {
				return intersect((Circle) e1, (Line) e2);
			}
		}

		return new Point[0];
	}

}
