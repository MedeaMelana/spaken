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

}
